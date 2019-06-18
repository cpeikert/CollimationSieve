{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Phase where

import           Data.DoubleWord
import           Data.DoubleWord.TH
import           Data.Int
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import           System.Random

mkUnpackedDoubleWord "Word320" ''Word64 "Int320" ''Int64 ''Word256 []

instance Random Word128 where
  {-# INLINABLE random #-}
  {-# INLINABLE randomR #-}

  random g = let (h,g')  = random g
                 (l,g'') = random g'
             in (Word128 h l, g'')

  randomR (lo,hi) g
    | lo > hi   = randomR (hi,lo) g
    | otherwise = let w@(Word128 h l) = hi - lo
                  in if h == 0
                     then let (r , g')  = randomR (0,l) g
                          in (lo + Word128 0 r, g')
                     else let (rh, g')  = randomR (0,h) g
                              (rl, g'') = random g'
                              r = Word128 rh rl
                          in if r > w then randomR (lo,hi) g'' else (lo+r, g'')

instance Random Word256 where
  {-# INLINABLE random #-}
  {-# INLINABLE randomR #-}

  random g = let (h,g')  = random g
                 (l,g'') = random g'
             in (Word256 h l, g'')

  randomR (lo,hi) g
    | lo > hi   = randomR (hi,lo) g
    | otherwise = let w@(Word256 h l) = hi - lo
                  in if h == 0
                     then let (r , g')  = randomR (0,l) g
                          in (lo + Word256 0 r, g')
                     else let (rh, g')  = randomR (0,h) g
                              (rl, g'') = random g'
                              r = Word256 rh rl
                          in if r > w then randomR (lo,hi) g'' else (lo+r, g'')

instance Random Word320 where
  {-# INLINABLE random #-}
  {-# INLINABLE randomR #-}

  random g = let (h,g')  = random g
                 (l,g'') = random g'
             in (Word320 h l, g'')

  randomR (lo,hi) g
    | lo > hi   = randomR (hi,lo) g
    | otherwise = let w@(Word320 h l) = hi - lo
                  in if h == 0
                     then let (r , g')  = randomR (0,l) g
                          in (lo + Word320 0 r, g')
                     else let (rh, g')  = randomR (0,h) g
                              (rl, g'') = random g'
                              r = Word320 rh rl
                          in if r > w then randomR (lo,hi) g'' else (lo+r, g'')

derivingUnbox "Word128"
    [t| Word128 -> (Word64, Word64) |]
    [| \ (Word128 h l) -> (h, l) |]
    [| uncurry Word128 |]

derivingUnbox "Word256"
    [t| Word256 -> (Word128, Word128) |]
    [| \ (Word256 h l) -> (h, l) |]
    [| uncurry Word256 |]

derivingUnbox "Word320"
    [t| Word320 -> (Word64, Word256) |]
    [| \ (Word320 h l) -> (h, l) |]
    [| uncurry Word320 |]

type Phase = Word320
