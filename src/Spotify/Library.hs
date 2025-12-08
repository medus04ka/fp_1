module Spotify.Library
  ( allTracks,
    demoUser,
    demoLikes,
    demoPlaylist,
    trackAudioPath
  )
where

import Music.Core
import Music.EDSL
import Spotify.Model
import Data.Maybe (Maybe(Just))

track1 :: Track
track1 =
  Track
    { trackId = 1,
      title = "Simple Intro",
      artist = "Functional Girl",
      genre = Classical,
      tags = ["intro", "calm", "simple"],
      musicData =
        c4 Quarter
          |+| d4 Quarter
          |+| e4 Half
    }

track2 :: Track
track2 =
  Track
    { trackId = 2,
      title = "Parallel Chord",
      artist = "Lambda Band",
      genre = Rock,
      tags = ["chord", "rock", "bright"],
      musicData =
        (c4 Quarter ||| e4 Quarter ||| g4 Quarter)
          |+| rest Eighth
          |+| c4 Eighth
    }

track3 :: Track
track3 =
  Track
    { trackId = 3,
      title = "Jazz-ish Line",
      artist = "Monadic Trio",
      genre = Jazz,
      tags = ["jazz", "line", "melodic"],
      musicData =
        d4 Eighth
          |+| e4 Eighth
          |+| g4 Quarter
          |+| e4 Quarter
    }

track4 :: Track
track4 =
  Track
    { trackId = 4,
      title = "Подозрение",
      artist = "Кровосток",
      genre = HipHop,
      tags = ["hiphop", "beat", "rhythm"],
      musicData =
        c4 Quarter
          |+| rest Eighth
          |+| c4 Eighth
          |+| g4 Half
    }

track5 :: Track
track5 =
  Track
    { trackId = 5,
      title = "Manchild",
      artist = "Sabrina Carpenter",
      genre = Pop,
      tags = ["pop", "spotify", "hit"],
      musicData =
        c4 Quarter
          |+| d4 Quarter
          |+| e4 Half
    }

allTracks :: [Track]
allTracks = [track1, track2, track3, track4, track5]

demoUser :: User
demoUser =
  User
    { userId = 1,
      userName = "pvc"
    }

demoLikes :: [Like]
demoLikes =
  [ Like {likeUser = userId demoUser, likeTrack = 1},
    Like {likeUser = userId demoUser, likeTrack = 3}
  ]

demoPlaylist :: Playlist
demoPlaylist =
  Playlist
    { playlistId = 1,
      owner = userId demoUser,
      name = "Demo Playlist",
      tracks = [1, 3]
    }

trackAudioPath :: TrackId -> Maybe FilePath
trackAudioPath 1 = Nothing
trackAudioPath 2 = Nothing
trackAudioPath 3 = Nothing
trackAudioPath 4 = Just "..\\krovostok-podozrenie.mp3"
trackAudioPath 5 = Just "https://open.spotify.com/track/2BwO5K8Q7EPAJSGze3AAh9?si=1752f560069a4735"
trackAudioPath _ = Nothing
