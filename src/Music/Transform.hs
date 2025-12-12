module Music.Transform
  ( transpose,
    stretchDuration,
  )
where

import Music.Core

transpose :: Int -> Music -> Music
transpose n m =
  case m of
    Single (Note pc (Octave o) d) ->
      let idx = fromEnum pc + n
          pc' = toEnum (idx `mod` 12)
       in Single (Note pc' (Octave o) d)
    Rest d ->
      Rest d
    Seq a b ->
      Seq (transpose n a) (transpose n b)
    Par a b ->
      Par (transpose n a) (transpose n b)

stretchDuration :: Rational -> Music -> Music
stretchDuration k m =
  case m of
    Single (Note pc o d) ->
      Single (Note pc o (scaleDuration k d))
    Rest d ->
      Rest (scaleDuration k d)
    Seq a b ->
      Seq (stretchDuration k a) (stretchDuration k b)
    Par a b ->
      Par (stretchDuration k a) (stretchDuration k b)

scaleDuration :: Rational -> Duration -> Duration
scaleDuration k d =
  case d of
    Whole -> Custom (4 * k)
    Half -> Custom (2 * k)
    Quarter -> Custom (1 * k)
    Eighth -> Custom (1 / 2 * k)
    Custom r -> Custom (r * k)
