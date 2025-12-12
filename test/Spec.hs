module Main (main) where

import qualified MusicSpec
import qualified SpotifySpec
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    MusicSpec.spec
    SpotifySpec.spec
