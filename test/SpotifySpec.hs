module SpotifySpec (spec) where

import Spotify.Engine
import Spotify.Model
import Test.Hspec

spec :: Spec
spec = do
  describe "Добавление треков в плейлисты" $ do
    it "Добавление треков по ид" $ do
      let pl =
            Playlist
              { playlistId = 1,
                owner = 1,
                name = "Test",
                tracks = [1, 2]
              }
          pl' = addTrackToPlaylist 3 pl
      tracks pl' `shouldBe` [1, 2, 3]

  describe "Удаление треков из плейлиста" $ do
    it "Удаление трека по ид" $ do
      let pl =
            Playlist
              { playlistId = 1,
                owner = 1,
                name = "Test",
                tracks = [1, 2, 3]
              }
          pl' = removeTrackFromPlaylist 2 pl
      tracks pl' `shouldBe` [1, 3]
