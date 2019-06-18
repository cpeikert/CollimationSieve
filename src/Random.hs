{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Random (CryptoRand, evalCryptoRandIO) where

import           Control.Arrow
import           Control.Monad.CryptoRandom
import           Control.Monad.IO.Class
import           Control.Monad.Random       (RandT, evalRandT)
import           Crypto.Random
import           Crypto.Random.DRBG
import           System.Random

-- | wrapper
newtype CryptoRand g = CryptoRand g deriving (CryptoRandomGen)

type DefaultDRBG = CryptoRand HashDRBG

-- | Evaluate a 'RandT' computation using the default cryptographic
-- generator, seeded by system entropy.  (Note that the updated
-- generator is not returned.)
evalCryptoRandIO :: MonadIO io => RandT DefaultDRBG io a -> io a
evalCryptoRandIO x = do
  gen <- liftIO newGenIO -- uses system entropy
  evalRandT x gen

-- | Turns a 'CryptoRandomGen' @g@ into a standard 'RandomGen'.
instance (CryptoRandomGen g) => RandomGen (CryptoRand g) where
  -- use 'CRandom' instance for 'Int'
  next (CryptoRand g) = either (error . show) (second CryptoRand) $ crandom g

  split (CryptoRand g) =
    either (error . show) (CryptoRand *** CryptoRand) $ splitGen g

  {-# INLINABLE next #-}
  {-# INLINABLE split #-}
