module Music.EDSL
  ( note,
    rest,
    (|+|),
    (|||),
    c4,
    d4,
    e4,
    g4,
  )
where

import Music.Core

note :: PitchClass -> Int -> Duration -> Music
note pc oct d = Single (Note pc (Octave oct) d)

rest :: Duration -> Music
rest = Rest

infixl 5 |+|

(|+|) :: Music -> Music -> Music
(|+|) = Seq

infixl 6 |||

(|||) :: Music -> Music -> Music
(|||) = Par

c4 :: Duration -> Music
c4 = note C 4

d4 :: Duration -> Music
d4 = note D 4

e4 :: Duration -> Music
e4 = note E 4

g4 :: Duration -> Music
g4 = note G 4
