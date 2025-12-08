module Spotify.Engine
  ( addTrackToPlaylist,
    removeTrackFromPlaylist,
    likesOfUser,
    recommendedByGenre,
  )
where

import qualified Data.Map.Strict as Map
import Spotify.Model

addTrackToPlaylist :: TrackId -> Playlist -> Playlist
addTrackToPlaylist tid pl =
  pl {tracks = tracks pl ++ [tid]}

removeTrackFromPlaylist :: TrackId -> Playlist -> Playlist
removeTrackFromPlaylist tid pl =
  pl {tracks = filter (/= tid) (tracks pl)}

likesOfUser :: UserId -> [Like] -> [TrackId]
likesOfUser uId ls =
  [likeTrack l | l <- ls, likeUser l == uId]

recommendedByGenre :: [Track] -> [Like] -> UserId -> [Track]
recommendedByGenre allTracks likes uId =
  let likedIds    = likesOfUser uId likes
      likedTracks = filter (\t -> trackId t `elem` likedIds) allTracks

      favGenres   = mostPopularGenres likedTracks
      favTags     = mostPopularTags likedTracks
   in filter (isRecommended favGenres favTags) allTracks

isRecommended :: [Genre] -> [String] -> Track -> Bool
isRecommended favGenres favTags t =
  genre t `elem` favGenres
    || not (null (filter (`elem` favTags) (tags t)))

mostPopularGenres :: [Track] -> [Genre]
mostPopularGenres ts =
  let counts =
        foldr
          (\t m -> Map.insertWith (+) (genre t) (1 :: Int) m)
          Map.empty
          ts
   in case Map.toList counts of
        [] -> []
        xs ->
          let maxCount = maximum (map snd xs)
           in [g | (g, c) <- xs, c == maxCount]

mostPopularTags :: [Track] -> [String]
mostPopularTags ts =
  let allTags = concatMap tags ts
      counts =
        foldr
          (\tag m -> Map.insertWith (+) tag (1 :: Int) m)
          Map.empty
          allTags
   in case Map.toList counts of
        [] -> []
        xs ->
          let maxCount = maximum (map snd xs)
           in [tag | (tag, c) <- xs, c == maxCount]