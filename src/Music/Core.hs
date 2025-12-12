module Music.Core
  ( PitchClass (..),
    Octave (..),
    Duration (..),
    Note (..),
    Music (..),
  )
where

data PitchClass
  = C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving (Show, Eq, Enum, Bounded)

newtype Octave = Octave Int
  deriving (Show, Eq, Ord)

data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Custom Rational
  deriving (Show, Eq)

data Note = Note
  { pitchClass :: PitchClass,
    octave :: Octave,
    duration :: Duration
  }
  deriving (Show, Eq)

data Music
  = Single Note
  | Rest Duration
  | Seq Music Music
  | Par Music Music
  deriving (Show, Eq)
