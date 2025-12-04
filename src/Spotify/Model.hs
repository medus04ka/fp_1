module Spotify.Model
  ( TrackId,
    UserId,
    PlaylistId,
    Genre (..),
    Track (..),
    User (..),
    Playlist (..),
    Like (..),
  )
where

import Music.Core (Music)

type TrackId = Int
type UserId = Int
type PlaylistId = Int

data Genre
  = Rock
  | Pop
  | Jazz
  | Classical
  | Electronic
  | Other String
  deriving (Show, Eq, Ord)

data Track = Track
  { trackId   :: TrackId
  , title     :: String
  , artist    :: String
  , genre     :: Genre
  , musicData :: Music
  }
  deriving (Show, Eq)

data User = User
  { userId   :: UserId
  , userName :: String
  }
  deriving (Show, Eq)

data Playlist = Playlist
  { playlistId :: PlaylistId
  , owner      :: UserId
  , name       :: String
  , tracks     :: [TrackId]
  }
  deriving (Show, Eq)

data Like = Like
  { likeUser  :: UserId
  , likeTrack :: TrackId
  }
  deriving (Show, Eq)
