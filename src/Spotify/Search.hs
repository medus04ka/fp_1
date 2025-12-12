{-# LANGUAGE OverloadedStrings #-}

module Spotify.Search
(searchTracks,
TrackHit(..),
trackHitUrl
) where

import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.URI (urlEncode)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import System.Environment (getEnv)

newtype TokenResponse = TokenResponse {
    accessToken :: T.Text 
    }

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o ->
    TokenResponse <$> o .: "access_token"

getAccessToken :: IO String
getAccessToken = do
  clientId <- getEnv "SPOTIFY_CLIENT_ID"
  clientSecret <- getEnv "SPOTIFY_CLIENT_SECRET"

  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "https://accounts.spotify.com/api/token"

  let auth = B.pack (clientId ++ ":" ++ clientSecret)
      authHeader = "Basic " <> B64.encode auth
      body = "grant_type=client_credentials"

      req =
        initReq
          { method = "POST"
          , requestHeaders =
              [ ("Authorization", authHeader)
              , ("Content-Type", "application/x-www-form-urlencoded")
              ]
          , requestBody = RequestBodyBS (B.pack body)
          }

  res <- httpLbs req manager
  case eitherDecode (responseBody res) of
    Right tr -> pure (T.unpack (accessToken tr))
    Left err -> fail ("Не удалось получить токен Spotify: " ++ err)

data TrackHit = TrackHit
  { hitName   :: String
  , hitArtist :: String
  , hitId     :: String
  } deriving (Show)

trackHitUrl :: TrackHit -> String
trackHitUrl h = "https://open.spotify.com/track/" ++ hitId h

data SearchResponse = SearchResponse { tracks :: TracksBlock }
data TracksBlock = TracksBlock { items :: [TrackItem] }
data TrackItem = TrackItem
  { tiName    :: T.Text
  , tiId      :: T.Text
  , tiArtists :: [ArtistItem]
  }
data ArtistItem = ArtistItem { aiName :: T.Text }

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \o ->
    SearchResponse <$> o .: "tracks"

instance FromJSON TracksBlock where
  parseJSON = withObject "TracksBlock" $ \o ->
    TracksBlock <$> o .: "items"

instance FromJSON TrackItem where
  parseJSON = withObject "TrackItem" $ \o ->
    TrackItem
      <$> o .: "name"
      <*> o .: "id"
      <*> o .: "artists"

instance FromJSON ArtistItem where
  parseJSON = withObject "ArtistItem" $ \o ->
    ArtistItem <$> o .: "name"

searchTracks :: String -> IO [TrackHit]
searchTracks query = do
  token <- getAccessToken
  manager <- newManager tlsManagerSettings

  let qEncoded = urlEncode True (TE.encodeUtf8 (T.pack query))
      url =
        "https://api.spotify.com/v1/search?type=track&limit=7&q="
          ++ B.unpack qEncoded

  initReq <- parseRequest url
  let req =
        initReq
          { requestHeaders =
              [ ("Authorization", B.pack ("Bearer " ++ token)) ]
          }

  res <- httpLbs req manager
  case eitherDecode (responseBody res) of
    Left err -> do
      putStrLn "Spotify вернул неожиданный JSON при поиске:"
      BL.putStr (responseBody res)
      putStrLn ""
      fail ("Ошибка парсинга поиска Spotify: " ++ err)
    Right (SearchResponse (TracksBlock its)) ->
      pure (map toHit its)
  where
    toHit :: TrackItem -> TrackHit
    toHit ti =
      TrackHit
        { hitName = T.unpack (tiName ti)
        , hitArtist =
            case tiArtists ti of
              (a:_) -> T.unpack (aiName a)
              []    -> "Unknown"
        , hitId = T.unpack (tiId ti)
        }
