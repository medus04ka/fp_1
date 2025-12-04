module Music.Analysis
  ( durationToBeats,
    totalDuration,
    pitchRange,
  )
where

import Music.Core

durationToBeats :: Duration -> Rational
durationToBeats d =
  case d of
    Whole -> 4
    Half -> 2
    Quarter -> 1
    Eighth -> 1 / 2
    Custom r -> r

totalDuration :: Music -> Rational
totalDuration m =
  case m of
    Single n ->
      durationToBeats (duration n)
    Rest d ->
      durationToBeats d
    Seq a b ->
      totalDuration a + totalDuration b
    Par a b ->
      max (totalDuration a) (totalDuration b)

pitchRange :: Music -> Maybe (Int, Int)
pitchRange m =
  case notesOf m of
    [] -> Nothing
    ns ->
      let ps = map pitchToInt ns
       in Just (minimum ps, maximum ps)

notesOf :: Music -> [Note]
notesOf m =
  case m of
    Single n -> [n]
    Rest _ -> []
    Seq a b -> notesOf a ++ notesOf b
    Par a b -> notesOf a ++ notesOf b

pitchToInt :: Note -> Int
pitchToInt (Note pc (Octave o) _) =
  let pcIndex = fromEnum pc
   in o * 12 + pcIndex
