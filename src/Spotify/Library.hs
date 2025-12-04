module Spotify.Library
  ( allTracks,
    demoUser,
    demoLikes,
    demoPlaylist,
  )
where

import Music.Core
import Music.EDSL
import Spotify.Model

track1 :: Track
track1 =
  Track
    { trackId = 1,
      title = "Simple Intro",
      artist = "Functional Girl",
      genre = Classical,
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
      musicData =
        d4 Eighth
          |+| e4 Eighth
          |+| g4 Quarter
          |+| e4 Quarter
    }

allTracks :: [Track]
allTracks = [track1, track2, track3]

demoUser :: User
demoUser =
  User
    { userId = 1,
      userName = "DemoUser"
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
