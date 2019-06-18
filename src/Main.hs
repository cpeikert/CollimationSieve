{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main (main)
where

import           Phase
import           Random

import           Control.Monad                     as CM
import qualified Control.Monad.Log                 as ML
import           Control.Monad.Random
import qualified Control.Monad.State.Strict        as MS
import           Data.Complex
import qualified Data.List                         as L
import qualified Data.Massiv.Array                 as A
import qualified Data.Massiv.Array.Manifest.Vector as AMV
import qualified Data.MultiSet                     as M
import qualified Data.Vector.Unboxed               as V
import           Numeric
import           Prelude                           as P
import           System.Console.ANSI
import           System.Environment
import           System.TimeIt

-- | a vector of multiplication phases; should be kept sorted
type PhaseVector = A.Array A.U A.Ix1 Phase

-- | the massiv computation strategy to use throughout
strat :: A.Comp
strat = A.Par

fromVector :: Vector Phase -> PhaseVector
fromVector vec = AMV.fromVector' strat (A.Sz1 (V.length vec)) vec

-- | a tuple of phase vectors to be collimated
type PVTuple = (PhaseVector, PhaseVector)

-- what LoggingT couldn't
deriving instance (MonadRandom m) => MonadRandom (ML.LoggingT message m)

-- | the base-2 logarithm
log2 :: Integral a => a -> Double
log2 = logBase (2 :: Double) . fromIntegral

-- | for convenience
log2rnd :: Integral a => a -> Int
log2rnd  = round . log2

dot :: Num a => [a] -> [a] -> a
dot [] _          = 0
dot _ []          = 0
dot (x:xs) (y:ys) = x*y + dot xs ys

---------- COLLIMATION ----------

data CollimateInfo = SI { s     :: !Phase, lReq :: !Int,
                          lReal :: !Int,   kept :: !Bool }

instance Show CollimateInfo where
  show SI{..} =
    let lRatio = fromIntegral lReal / fromIntegral lReq :: Double
    in "(~2^" P.++ show (log2rnd s) P.++
       "," P.++ show lReq P.++
       "," P.++ show lReal P.++
       "," P.++ showFFloat (Just 2) lRatio
       (if not kept then " DISCARDED" else "") P.++ ")"

collimateInfoHandler :: MonadIO m
                     => Int     -- | number of bottom sieve layers to suppress
                     -> [Phase] -- | list of intervals sizes for sieve
                     -> CollimateInfo
                     -> m ()
collimateInfoHandler skip ss =
  let len = P.length ss in \si@SI{..} ->
    let i' = L.elemIndex s ss
    in maybe (return ())
       (\i -> when (i < len - skip) $ liftIO $ do
           setSGR [SetColor Foreground (if i < 7 then Vivid else Dull)
                   (toEnum (i `mod` 7 + 1))]
           putStrLn $ P.concat (P.replicate i "  ") P.++ show si
           setSGR [Reset]) i'

-- | all the mod-@n@ subset-sums of a vector. Results are not
-- necessarily sorted!
subsetSums :: Vector Phase -> Vector Phase
subsetSums v =
  V.generate (2^V.length v) (go 0 0) where
  go i acc s
    | i >= V.length v = acc
    | otherwise = go (i+1) (acc + if odd s then v V.! i else 0) (s `div` 2)

-- | choose a random phase multiplier from the given phase vector
randomElt :: MonadRandom m => PhaseVector -> m Phase
randomElt v =  (v A.!) <$> getRandomR (0, A.elemsCount v - 1)

-- | the first index (in a sorted vector) of an element that is /at
-- least/ @a@, or 'length v' if none exists
findAtLeast :: PhaseVector -> Phase -> Int
findAtLeast v a = go 0 (A.elemsCount v) where
  go i j | i == j         = i
         | (v A.! i) >= a = i
         -- don't use i+j `div` 2, to avoid overflow on big arrays
         | otherwise      = let k = i + ((j - i) `div` 2)
                            in if (v A.! k) >= a then go i k else go (k+1) j

-- | collimate phase vectors
collimate :: (MonadRandom m)
          => Phase              -- | desired interval size S (upper bound)
          -> PVTuple            -- | phase vectors to collimate
          -> m PhaseVector
collimate s (v1,v2) = do
  b1 <- randomElt v1
  b2 <- randomElt v2
  let q = (b1 + b2) `div` s
      qs = q*s
      start i1 = (i1, findAtLeast v2 (qs - (v1 A.! i1)))
      gen (i1,i2)
        | i1 >= A.elemsCount v1 = Nothing
        | i2 >= A.elemsCount v2 = gen $ start (i1+1)
        | v < qs + s            = Just (v `mod` s, (i1, i2+1))
        | otherwise             = gen $ start (i1+1)
        where v = (v1 A.! i1) + (v2 A.! i2)
      -- Create a Vector because massiv doesn't supported
      -- unknown-length unfoldr.
  return $ A.quicksort $ fromVector $ V.unfoldr gen (start 0)

---------- SIEVE ----------

data SieveState = SS { numQueries :: !Int, maxLength    :: !Int,
                       numNodes   :: !Int, numDiscarded :: !Int}
  deriving (Show, Eq)

newSieveState :: SieveState
newSieveState = SS 0 0 0 0

addQueries :: Int -> SieveState -> SieveState
addQueries i ss@SS{..} = ss { numQueries = numQueries + i }

updateLength :: Int -> SieveState -> SieveState
updateLength l ss@SS{..} = ss { maxLength = max l maxLength }

incrementNodes :: SieveState -> SieveState
incrementNodes ss@SS{..} = ss { numNodes = succ numNodes }

incrementDiscarded :: SieveState -> SieveState
incrementDiscarded ss@SS{..} = ss { numDiscarded = succ numDiscarded }

-- | Collimation sieve.
sieve :: (MonadRandom m, ML.MonadLog CollimateInfo m,
          MS.MonadState SieveState m)
      => Phase   -- | group order N
      -> Double  -- | threshold factor for long-enough phase vectors
      -> [Phase] -- | increasing interval sizes S_i for the i'th level
                 -- of the sieve; the final one should equal N
      -> Int     -- | desired phase vector length L
      -> m PhaseVector
sieve = sieve' True where       -- don't discard final output
  sieve' _ _ _ [] _ = error "sieve: empty list of interval sizes"
  sieve' alwaysKeep n threshold ss@(s:ss') l
    | s >= n = do
        let logl = log2rnd l
        v <- (A.quicksort . fromVector . V.map (`mod` n)) . subsetSums <$>
          -- create a Vector because massiv doesn't support monadic replicate
             V.replicateM logl (getRandomR (0,n-1))
        ML.logMessage (SI s l (A.elemsCount v) True)
        MS.modify'   incrementNodes
        MS.modify' $ addQueries logl
        MS.modify' $ updateLength (A.elemsCount v)
        return v
    | otherwise =
        let s' = P.head ss'
            z  = 1.5 * fromIntegral l * fromIntegral s' / fromIntegral s :: Double
            l' = ceiling $ sqrt z
        in do v1   <- sieve' False n threshold ss' l'
              v2   <- sieve' False n threshold ss' $
                      ceiling (z / fromIntegral (A.elemsCount v1))
              v    <- collimate s (v1,v2)
              let vlen = A.elemsCount v
                  -- check if vector is long enough to be useful
                  keep = alwaysKeep ||
                         fromIntegral vlen / fromIntegral l >= threshold
              ML.logMessage (SI s l vlen keep)
              MS.modify' incrementNodes
              if keep
                then MS.modify' (updateLength vlen) >> return v
                else MS.modify' incrementDiscarded >> sieve' False n threshold ss l

-- | histogram of (n, # multipliers appearing n times) in a phase
-- vector for a (final, small) interval [S]
histogram :: Int -> Vector Int -> [(Int,Int)]
histogram s v =
  let occurs = fmap snd $ M.toOccurList $ M.fromList $ V.toList v
      num    = P.length occurs
      hist   = L.sortOn fst $ M.toOccurList $ M.fromList occurs
  in if s == num then hist else (0, s - num) : hist

chi :: Double -> Complex Double
chi = cis . (2.0 * pi *)

square :: Num a => a -> a
square x = x*x

-- | partition a phase vector into one consisting of its unique phase
-- multipliers, and one containing all the leftovers.
puncture :: PhaseVector -> (PhaseVector, PhaseVector)
puncture v =
  let v' = AMV.toVector v
      u' = V.uniq v'
      gen i
        | i >= V.length v' = Nothing
        | v' V.! (i-1) == v' V.! i = Just (v' V.! i, i+1)
        | otherwise = gen $ i+1
  in (fromVector u', fromVector $ V.unfoldr gen 1)

unfold :: (b -> (a,b)) -> b -> [a]
unfold f b = let (a,b') = f b in a : unfold f b'

-- | probability assigned to \( w \), given by \( \theta = s/N - w/T
-- \), by the \( T \)-dimensional inverse-QFT of the given punctured
-- phase vector on \( [T] \).
probTheta :: Int                -- | range \( [T] \) of phase vector
          -> PhaseVector        -- | uniquified phase vector
          -> Double             -- | \( \theta \)
          -> Double
probTheta t v theta =
  square (magnitude $ A.sum $ A.map (chi . (* theta) . fromIntegral) v) /
  (fromIntegral t * fromIntegral (A.elemsCount v))

-- | probability that the \( T \)-dimensional inverse-QFT on the given
-- punctured phase vector on \( [T] \) will output some \( w \in
-- \lfloor sT/N \rceil - \{ - \lfloor z/2 \rfloor, \ldots, \lfloor
-- (z-1)/2 \rfloor \} \), given the (essentially uniform) shift \(
-- sT/N \bmod 1 \in [-1/2,1/2) \).  Assumes that (\ z \) is positive
-- and small enough not to double-count probabilities.
probClosest :: Int               -- | \( T \)
            -> Double            -- | \( sT/N \bmod 1 \in [-1/2,1/2) \)
            -> PhaseVector       -- | uniquified phase vector
            -> Int               -- | number \( z \) of closest to count
            -> Double
probClosest t shift v z =
  let thetas = A.makeArrayR A.D strat (A.Sz1 z)
               (\(A.Ix1 i) -> (shift + fromIntegral (i - z `div` 2))
                              / fromIntegral t)
  in A.sum $ A.map (probTheta t v) thetas

main :: IO ()
main = do
  args <- getArgs
  let (params,rest) = splitAt 3 args
      [logn :: Int, logl, logs] = read <$> params
      threshold = if null rest then 0.25 :: Double else read (head rest)
      n     = if logn > 0
              then 2^logn
                   -- from https://eprint.iacr.org/2019/498.pdf
              else 3 * 37 * 1407181 * 51593604295295867744293584889 *
                   31599414504681995853008278745587832204909
      l     = 2^logl
      s     = 2^logs
      ss    = P.takeWhile (< n)
              (iterate (\t -> (2 * t * fromIntegral l) `div` 3) $ fromIntegral s)
              P.++ [n]

  -- print schedule of log interval sizes
  putStrLn $ "log S's = " P.++ show (log2rnd <$> ss)

  -- run the sieve
  (time, (v, sieveState@SS{..})) <- timeItT $
    evalCryptoRandIO
    (flip ML.runLoggingT (collimateInfoHandler (min 5 (P.length ss - 1)) ss)
     (flip MS.runStateT newSieveState (sieve n threshold ss l)))

  -- print results

  -- parameters again, for convenience
  putStrLn $ "\n[log N, log L, log S_0] = " P.++
    show [logn, logl, logs]

  putStrLn $ "threshold = " P.++ showFFloat (Just 2) threshold ""

  -- log interval sizes again, for convenience
  putStrLn $ "\nlog S's = " P.++ show (log2rnd <$> ss)

  putStrLn $ "\nSieve summary = " P.++ show sieveState

  -- compute number of queries according to model
  let delta = fromIntegral numDiscarded / fromIntegral numNodes :: Double
      depth = P.length ss - 1
      l' = sqrt $ 1.5 * fromIntegral l * fromIntegral n /
           fromIntegral (ss P.!! (P.length ss - 2)) :: Double
      modelQueries = (2.0 / (1-delta))^depth * logBase 2 l'

  putStrLn $ "\nProbability of discarding = " P.++ showFFloat (Just 4) delta ""

  putStrLn $ (P.++) "\nNumber of queries actual/modeled = " $ (P.++)
    (show numQueries)                $ (P.++) "/" $
    showFFloat (Just 1) modelQueries $ (P.++) " ~= " $
    showFFloat (Just 2) (fromIntegral numQueries / modelQueries) ""

  -- compute probability of obtaining a regular state
  let hist = histogram s $ V.map fromIntegral $ AMV.toVector v
      num  = s * fst (P.head hist)
      den  = A.elemsCount v
      probRegular = fromIntegral num / fromIntegral den :: Double

  putStrLn $ "\nProbability of obtaining a regular state = " P.++
    show num P.++ "/" P.++ show den P.++ " ~= " P.++
    showFFloat (Just 3) probRegular ""

  -- compute probabilities of getting punctured regular states, and close
  shift :: Double <- getRandomR (-0.5, 0.5)
  let punctureds = takeWhile ((> 2^(logs - 6)) . A.elemsCount) $
                   unfold puncture v
      puncturedProbs :: [Double] =
        (/ fromIntegral den) . fromIntegral . A.elemsCount <$> punctureds
      closest = [1,2,4,8]       -- how many of the closest w to check
      closestProbs = map (<$> closest) (probClosest s shift <$> punctureds)
      totalClosestProbs = dot puncturedProbs <$> L.transpose closestProbs

  putStrLn $ (P.++) "\nProbability of obtaining each punctured state (1st, 2nd, ... attempt) =\n" $
    L.intercalate ", " $ mapM (showFFloat (Just 3)) puncturedProbs ""

  putStrLn $ (P.++) "\nProbability bounds for each punctured state for " $
    (P.++) (show closest) $ (P.++) " closest w = \n" $
    L.intercalate "\n" $ L.intercalate ", " <$>
    (mapM . mapM) (showFFloat (Just 3)) closestProbs ""

  putStrLn $ (P.++) "\nTotal probability bounds for " $
    (P.++) (show closest) $ (P.++) " closest w = \n" $
    L.intercalate ", " $ mapM (showFFloat (Just 3)) totalClosestProbs ""

  putStrLn $ (P.++) "\nLaTeX table row:\n" $
    showFFloat (Just 1) (log2 n)                          $ (P.++) " & " $
    showFFloat (Just 1) (log2 numQueries)                 $ (P.++) " & " $
    showFFloat (Just 1) (logBase 2 modelQueries)          $ (P.++) " & " $
    showFFloat (Just 1) (log2 maxLength)                  $ (P.++) " & " $
    (P.++) (show logl)                                    $ (P.++) " & " $
    (P.++) (show logs)                                    $ (P.++) " & " $
    showFFloat (Just 0) (probRegular * 100)               $ (P.++) " & " $
    showFFloat (Just 1) (fromIntegral logs * probRegular) $ (P.++) " & " $
    showFFloat (Just 2) threshold                         $ (P.++) " & " $
    showFFloat (Just 1) (delta * 100)                     $ (P.++) " & " $
    (P.++) (show depth)                                   $ (P.++) " & " $
    showFFloat (Just 1) (time / 3600) " \\\\"

