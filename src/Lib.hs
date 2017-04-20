{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Data.VectorSpace
import Data.Maybe (fromJust)
import Control.Applicative
import Data.List

import Utils

-- Me being pretentious
type ℤ = Int

data EqClass v = EqClass [v] deriving Show

-- TODO move the second argument to type level so adding two quotient spaces
-- only makes sense when they have the same quotient
-- Also we store a lot of redundant information with current setup
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Pitch = QuotientSpace ℤ (EqClass ℤ)

type Chord = [Pitch]

newtype PitchDist = PitchDist ℤ deriving (Num, Ord, Eq, Show)
newtype ChordDisplace = ChordDisplace [PitchDist] deriving Show

-- Measure the distance from a given note to the closest C note
pitchClosestZero :: Pitch -> PitchDist
pitchClosestZero p = min dist dist'
  where
    note  = getElem p
    EqClass octaves = getQuotient p
    below  = fromJust $ firstBelow octaves note
    above  = fromJust $ firstAbove octaves note
    dist = PitchDist $ note - below
    dist' = PitchDist $ above - note

-- Get distance between notes modulo octave
pitchDist :: Pitch -> Pitch -> PitchDist
pitchDist p p' = abs $ pitchClosestZero p - pitchClosestZero p'

-- Get the displacement multiset from the two chords
chordDist :: Chord -> Chord -> ChordDisplace
chordDist = (ChordDisplace .) . (<*>) . (pitchDist <$>)

type VoiceLeadMetric = Chord -> Chord -> PitchDist
-- type Metric = 
  -- forall a t. (Ord a, Num a, Functor t, Foldable t) => t a -> t a -> a

voSmooth :: VoiceLeadMetric
voSmooth c c' = sum $ zipWith pitchDist c c'

-- Defined only for n ∈ ℤ+
-- Add L∞? (Max)
voVectorNorm :: ℤ -> VoiceLeadMetric
voVectorNorm n c c' = if n > 0 
  then sum $ zipWith (\x y-> (pitchDist x y) ^ n) c c'
  else undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

octaves :: EqClass ℤ
octaves = EqClass $ iterate (+12) 0 

note :: ℤ -> Pitch
note = (flip QuotientSpace) octaves

middleC = note (4 * 12)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Progression = (Chord, Chord)

minProgression :: VoiceLeadMetric -> Chord -> Chord -> Progression
minProgression m c c' = foldl f start ccs
  where
    start = ([], [])
    f p = (bestProgression m) . (uncurry (buildColRow p))
    cs = tail $ inits c
    cs' = tail $ inits c'
    ccs = zip cs cs'

bestProgression :: VoiceLeadMetric -> [Progression] -> Progression
bestProgression m ps = snd $ foldl f startProgression list
  where
    startProgression = (10000, ([], []))
    list = zip (map (uncurry m) ps) ps
    f (d, x) (d', y) = if d < d' then (d, x) else (d', y)

buildColRow :: Progression -> Chord -> Chord -> [Progression]
buildColRow (progFrom, progTo) c c' = col ++ rowRest
  where
    col :: [Progression]
    col = map (\n -> (progFrom ++ [n], progTo ++ [last c'])) c
    (_ : rowRest) = map (\n -> (progFrom ++ [last c], progTo ++ [n])) c'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Debug
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

testProg :: Progression
testProg = (map (note . (+48)) [4, 7, 0, 11], map (note . (+48)) [4, 8, 11, 3])
test = (map (note . (+48)) [4, 7], map (note . (+48)) [4, 8])

showProg :: Progression -> String
showProg (from, to) = "(" ++ fst ++ ") -> (" ++ snd ++ ")"
  where
    fst = intercalate ", " $ map shw from 
    snd = intercalate ", " $ map shw to

shw :: (Show a) => QuotientSpace a (EqClass a) -> String
shw = show . getElem
