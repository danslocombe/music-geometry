{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.VectorSpace
import Data.Maybe (fromJust)
import qualified Data.MultiSet as MS

import Utils

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data EqClass v = EqClass [v] deriving Show

data QuotientSpace a b where 
  QuotientSpace :: VectorSpace a => {
    getElem :: a,
    getQuotient :: EqClass a
  } -> QuotientSpace a (EqClass a)

instance AdditiveGroup (QuotientSpace a b) where
  zeroV = zeroV
  (^+^) (QuotientSpace x y) (QuotientSpace x' y') = QuotientSpace (x ^+^ x') y
  negateV (QuotientSpace x y) = QuotientSpace (negateV x) y

instance VectorSpace (QuotientSpace a b) where
  type Scalar (QuotientSpace a b) = Scalar a
  (*^) λ (QuotientSpace x y) = QuotientSpace (λ *^ x) y

type Pitch = QuotientSpace Int (EqClass Int)

octaves :: EqClass Int
octaves = EqClass $ iterate (+12) 0 

note :: Int -> Pitch
note = (flip QuotientSpace) octaves

middleC = note (4 * 12)

newtype PitchDist = PitchDist Int deriving (Num, Ord, Eq, Show)

pitchClosestZero :: Pitch -> PitchDist
pitchClosestZero p = min dist dist'
  where
    note  = getElem p
    EqClass octaves = getQuotient p
    below  = fromJust $ firstBelow octaves note
    above  = fromJust $ firstAbove octaves note
    dist = PitchDist $ note - below
    dist' = PitchDist $ above - note

pitchDist :: Pitch -> Pitch -> PitchDist
pitchDist p p' = abs $ pitchClosestZero p - pitchClosestZero p'

type Chord = MS.MultiSet Pitch
