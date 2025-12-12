-- {-# LANGUAGE OverloadedStrings #-}

module Spotify.External
  -- ( analyzeSpotifyTrack
  -- ) 
  where

-- import Network.HTTP.Client
-- import Network.HTTP.Client.TLS
-- import Data.Aeson
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Base64 as B64
-- import qualified Data.Text as T
-- import System.Environment (getEnv)
-- import Data.List (isPrefixOf, tails)


-- extractTrackId :: String -> Maybe String
-- extractTrackId url =
--   case dropWhile (not . isPrefixOf "track/") (tails url) of
--     (x : _) ->
--       let rest  = drop 6 x
--           ident = takeWhile (\c -> c /= '?' && c /= '&' && c /= ' ') rest
--        in if null ident then Nothing else Just ident
--     [] -> Nothing

-- data TokenResponse = TokenResponse
--   { accessToken :: T.Text
--   }
--   deriving (Show)

-- instance FromJSON TokenResponse where
--   parseJSON = withObject "TokenResponse" $ \o ->
--     TokenResponse <$> o .: "access_token"

-- getAccessToken :: IO String
-- getAccessToken = do
--   clientId     <- getEnv "SPOTIFY_CLIENT_ID"
--   clientSecret <- getEnv "SPOTIFY_CLIENT_SECRET"

--   manager   <- newManager tlsManagerSettings
--   initReq   <- parseRequest "https://accounts.spotify.com/api/token"

--   let auth       = B.pack (clientId ++ ":" ++ clientSecret)
--       authHeader = "Basic " <> B64.encode auth
--       body       = "grant_type=client_credentials"

--       req =
--         initReq
--           { method         = "POST"
--           , requestHeaders =
--               [ ("Authorization", authHeader)
--               , ("Content-Type", "application/x-www-form-urlencoded")
--               ]
--           , requestBody    = RequestBodyBS (B.pack body)
--           }

--   res <- httpLbs req manager

--   case eitherDecode (responseBody res) of
--     Right tr  -> return (T.unpack (accessToken tr))
--     Left  err -> fail ("Ошибка при разборе JSON токена: " ++ err)

-- data AudioFeatures = AudioFeatures
--   { tempo  :: Double
--   , keyVal :: Int
--   , modeVal :: Int
--   , energy :: Double
--   , danceability :: Double
--   , valence :: Double
--   , acousticness :: Double
--   , instrumentalness :: Double
--   , loudness :: Double
--   , durationMs :: Int
--   } deriving Show

-- instance FromJSON AudioFeatures where
--   parseJSON = withObject "AudioFeatures" $ \o ->
--     AudioFeatures
--       <$> o .: "tempo"
--       <*> o .: "key"
--       <*> o .: "mode"
--       <*> o .: "energy"
--       <*> o .: "danceability"
--       <*> o .: "valence"
--       <*> o .: "acousticness"
--       <*> o .: "instrumentalness"
--       <*> o .: "loudness"
--       <*> o .: "duration_ms"


-- fetchAudioFeatures :: String -> IO AudioFeatures
-- fetchAudioFeatures trackId = do
--   token   <- getAccessToken
--   manager <- newManager tlsManagerSettings

--   initReq <- parseRequest $
--     "https://api.spotify.com/v1/audio-features/" ++ trackId

--   let req =
--         initReq
--           { requestHeaders =
--               [ ("Authorization", B.pack ("Bearer " ++ token)) ]
--           }

--   res <- httpLbs req manager

--   case eitherDecode (responseBody res) of
--     Right af -> return af
--     Left err -> do
--       putStrLn "Не удалось разобрать JSON audio-features. Сырой ответ от Spotify:"
--       BL.putStr (responseBody res)
--       putStrLn ""
--       fail ("Ошибка разбора JSON audio-features: " ++ err)

-- analyzeSpotifyTrack :: String -> IO ()
-- analyzeSpotifyTrack url = do
--   putStrLn "Пытаюсь извлечь track id из ссылки..."
--   case extractTrackId url of
--     Nothing -> putStrLn "Не удалось извлечь track id из URL Spotify."
--     Just tid -> do
--       putStrLn $ "Track id: " ++ tid
--       putStrLn "Запрашиваю аудио-характеристики в Spotify..."
--       af <- fetchAudioFeatures tid
--       putStrLn "\nАнализ трека"
--       putStrLn ("Темп: " ++ show (tempo af))
--       putStrLn ("Ключ: " ++ show (keyVal af))
--       putStrLn ("Режим: " ++ if modeVal af == 1 then "Мажор" else "Минор")
--       putStrLn ("Энергия: " ++ show (energy af))
--       putStrLn ("Танцевальность: " ++ show (danceability af))
--       putStrLn ("Позитивность: " ++ show (valence af))
--       putStrLn ("Акустичность: " ++ show (acousticness af))
--       putStrLn ("Инструментальность: " ++ show (instrumentalness af))
--       putStrLn ("Громкость (дБ): " ++ show (loudness af))
--       putStrLn ("Длительность (мс): " ++ show (durationMs af))
--       putStrLn "-------------------------------"