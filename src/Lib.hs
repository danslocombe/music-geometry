{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.VectorSpace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Kernel v = Kernel [v]

data QuotientSpace a b where 
  QuotientSpace :: (VectorSpace a) => a -> Kernel a -> QuotientSpace a (Kernel a)

instance AdditiveGroup (QuotientSpace a b) where
  zeroV = zeroV
  (^+^) (QuotientSpace x y) (QuotientSpace x' y') = QuotientSpace (x ^+^ x') y
  negateV (QuotientSpace x y) = QuotientSpace (negateV x) y

instance VectorSpace (QuotientSpace a b) where
  type Scalar (QuotientSpace a b) = Scalar a
  (*^) λ (QuotientSpace x y) = QuotientSpace (λ *^ x) y
