module MusicSpec (spec) where

import Music.Core
import Music.Transform
import Test.Hspec
import Test.QuickCheck

instance Arbitrary PitchClass where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Octave where
  arbitrary = Octave <$> choose (0, 8)

instance Arbitrary Duration where
  arbitrary = elements [Whole, Half, Quarter, Eighth]

instance Arbitrary Note where
  arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Music where
  arbitrary = sized genMusic

genMusic :: Int -> Gen Music
genMusic n =
  if n <= 0
    then oneof [Single <$> arbitrary, Rest <$> arbitrary]
    else
      oneof
        [ Single <$> arbitrary,
          Rest <$> arbitrary,
          Seq <$> genMusic (n `div` 2) <*> genMusic (n `div` 2),
          Par <$> genMusic (n `div` 2) <*> genMusic (n `div` 2)
        ]

spec :: Spec
spec = do
  describe "Транспорировать(???)" $ do
    it "транспонирование на 0 оставляет музыку без изменений" $
      property $ \m ->
        transpose 0 (m :: Music) == m
