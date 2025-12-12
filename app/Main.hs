module Main (main) where

import Music.Analysis (pitchRange, totalDuration)
import Spotify.Engine
  ( addTrackToPlaylist,
    likesOfUser,
    recommendedByGenre,
    removeTrackFromPlaylist
  )
import Spotify.Library
  ( allTracks,
    demoLikes,
    demoPlaylist,
    demoUser,
    trackAudioPath
  )
import Spotify.Model
  ( Track(..),
    TrackId,
    Playlist(..),
    Like(..),
    User(..),
    Genre(..)
  )
import Spotify.Search(
    searchTracks,
    TrackHit(..),
    trackHitUrl
  )

import System.Info (os)
import System.Process (callCommand)
import Data.List (isInfixOf, dropWhileEnd)
import Data.Char (ord, isSpace)
import System.IO (hFlush, stdout)
import Control.Exception (try, IOException)
import System.Directory (doesFileExist)

-- TODO: а можно свои треки? переделать
-- и еще хочется чтоб они песни проигрывались, но это потом
-- чтоб меню не возникало каждый раз после выбора\\а было что типа "назад в меню"

data ExternalPlaylist = ExternalPlaylist
  { epName :: String,
    epUrls :: [String]
  }
  deriving (Show)

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Spotify Demo!"
  putStrLn ""
  loop demoLikes demoPlaylist Nothing

--loop сосяет с меню и операциями
loop :: [Like] -> Playlist -> Maybe ExternalPlaylist -> IO ()
loop likes pl extPl = do
  putStrLn ""
  putStrLn "Меню:"
  putStrLn "1. Показать все треки"
  putStrLn "2. Показать плейлист"
  putStrLn "3. Добавить трек в плейлист"
  putStrLn "4. Удалить трек из плейлиста"
  putStrLn "5. Поставить лайк треку"
  putStrLn "6. Показать лайкнутые треки"
  putStrLn "7. Показать рекомендации (по жанру)"
  putStrLn "8. Проанализировать трек"
  putStrLn "9. Проиграть трек"
  putStrLn "10. Импортировать плейлист из файла"
  putStrLn "11. Показать импортированный плейлист"
  putStrLn "12. Проиграть импортированный трек"
  putStrLn "13. Проанализировать трек из импортированного плейлиста"
  putStrLn "14. Поиск трека в Spotify"
  putStrLn "0. Выйти"
  putStrLn "================"
  putStr "Введите выбор: "
  hFlush stdout
  choice <- getLine
  putStrLn ""
  case choice of
    "1" -> do
      showAllTracks
      waitForEnter
      loop likes pl extPl
    "2" -> do
      showPlaylist pl
      waitForEnter
      loop likes pl extPl
    "3" -> do
      pl' <- addTrackToPlaylistIO pl
      waitForEnter
      loop likes pl' extPl
    "4" -> do
      pl' <- removeTrackFromPlaylistIO pl
      waitForEnter
      loop likes pl' extPl
    "5" -> do
      likes' <- likeTrackIO likes
      waitForEnter
      loop likes' pl extPl
    "6" -> do
      showLikedTracks likes
      waitForEnter
      loop likes pl extPl
    "7" -> do
      showRecommendations likes
      waitForEnter
      loop likes pl extPl
    "8" -> do
      analyzeTrackIO
      waitForEnter
      loop likes pl extPl
    "9" -> do
      playTrackIO
      waitForEnter
      loop likes pl extPl
    "10" -> do
      extPl' <- importExternalPlaylistIO
      waitForEnter
      loop likes pl extPl'
    "11" -> do
      showExternalPlaylist extPl
      waitForEnter
      loop likes pl extPl
    "12" -> do
      playFromExternalPlaylistIO extPl
      waitForEnter
      loop likes pl extPl
    "13" -> do
      analyzeFromExternalPlaylistIO extPl
      waitForEnter
      loop likes pl extPl
    "14" -> do
      searchAndSaveSpotifyLinkIO
      waitForEnter
      loop likes pl extPl
    "0" -> do
      putStrLn "Пока- пока!! Спасибо за использование Haskell Spotify Demo."
    _ -> do
      putStrLn "Чета ты набредил, попробуй еще раз. (только без бредика)"
      loop likes pl extPl

--вывод треков/ плейлистов
showAllTracks :: IO ()
showAllTracks = do
  putStrLn "Треки в библиотеке:"
  mapM_ printTrack allTracks

printTrack :: Track -> IO ()
printTrack t =
  putStrLn $
    show (trackId t)
      ++ " - "
      ++ title t
      ++ " ("
      ++ artist t
      ++ ", "
      ++ show (genre t)
      ++ ")"

showPlaylist :: Playlist -> IO ()
showPlaylist pl = do
  putStrLn $ "Плейлист: " ++ name pl
  putStrLn $ "Владелец (user id): " ++ show (owner pl)
  if null (tracks pl)
    then putStrLn "Плейлист пуст."
    else do
      putStrLn "Треки:"
      mapM_ printTrackInPlaylist (tracks pl)

printTrackInPlaylist :: TrackId -> IO ()
printTrackInPlaylist tid =
  case findTrack tid of
    Nothing ->
      putStrLn $ "- " ++ show tid ++ " (че? такого нету, блин НОТ ФАУНД)"
    Just t ->
      putStrLn $
        "- "
          ++ show tid
          ++ ": "
          ++ title t
          ++ " ("
          ++ artist t
          ++ ", "
          ++ show (genre t)
          ++ ")"

findTrack :: TrackId -> Maybe Track
findTrack tid =
  case filter (\t -> trackId t == tid) allTracks of
    []    -> Nothing
    t : _ -> Just t

--операции с плейлистом
addTrackToPlaylistIO :: Playlist -> IO Playlist
addTrackToPlaylistIO pl = do
  putStrLn "Вводите ид трека для добавления:"
  tid <- readIntFromLine
  case findTrack tid of
    Nothing -> do
      putStrLn "ноу сач трек ид ин либрари."
      return pl
    Just _ -> do
      let pl' = addTrackToPlaylist tid pl
      putStrLn "Трек добавлен в плейлист."
      return pl'

removeTrackFromPlaylistIO :: Playlist -> IO Playlist
removeTrackFromPlaylistIO pl = do
  putStrLn "Введи ид трека для удаления:"
  tid <- readIntFromLine
  if tid `elem` tracks pl
    then do
      let pl' = removeTrackFromPlaylist tid pl
      putStrLn "Трек удален из плейлиста."
      return pl'
    else do
      putStrLn "Такого трека в плейлисте нет."
      return pl

--Лайки и рекомендации
likeTrackIO :: [Like] -> IO [Like]
likeTrackIO likes = do
  let u = demoUser
  putStrLn $ "Курент юзер: " ++ userName u
  putStrLn "Ввести ид трека для лайка:"
  tid <- readIntFromLine
  case findTrack tid of
    Nothing -> do
      putStrLn "ноу сач трек ид ин либрари."
      return likes
    Just t -> do
      let alreadyLiked =
            any (\l -> likeUser l == userId u && likeTrack l == tid) likes
      if alreadyLiked
        then do
          putStrLn "Ты уже лайкнул этот трек, ЛАЙКАТЬ НЕ НАДО."
          return likes
        else do
          let likeEntry =
                Like
                  { likeUser = userId u,
                    likeTrack = tid
                  }
          putStrLn $
            "Лайкнутый трек: " ++ title t ++ " от " ++ artist t
          return (likeEntry : likes)

showLikedTracks :: [Like] -> IO ()
showLikedTracks likes = do
  let u        = demoUser
      likedIds = likesOfUser (userId u) likes
  putStrLn $ "Лайкнутый трек(и?) юзера " ++ userName u ++ ":"
  if null likedIds
    then putStrLn "Пока нет лайкнутых треков. тут желательно лайкнуть что-нибудь"
    else mapM_ printTrackInPlaylist likedIds

showRecommendations :: [Like] -> IO ()
showRecommendations likes = do
  let u    = demoUser
      recs = recommendedByGenre allTracks likes (userId u)
  putStrLn $ "Рекомендации для " ++ userName u ++ " (по жанру):"
  if null recs
    then putStrLn "Пока нет рекомендаций."
    else mapM_ printTrack recs

--Анализ трека
analyzeTrackIO :: IO ()
analyzeTrackIO = do
  putStrLn "Нужен ид трека для анализа:"
  tid <- readIntFromLine
  case findTrack tid of
    Nothing ->
      putStrLn "ноу сач трек ид ин либрари."
    Just t -> do
      case trackAudioPath tid of
        Just url | "open.spotify.com" `isInfixOf` url -> do
          putStrLn $ "Трек привязан к Spotify: " ++ url
          analyzeSpotifyLike t url
        _ -> do
          let m     = musicData t
              beats = totalDuration m
              pr    = pitchRange m
          putStrLn $ "Название: " ++ title t
          putStrLn $ "Исполнитель: " ++ artist t
          putStrLn $ "Жанр: " ++ show (genre t)
          putStrLn $ "Длительность (в долях): "
            ++ show (fromRational beats :: Double)
          case pr of
            Nothing ->
              putStrLn "Диапазон высот: нет нот (только паузы)."
            Just (lo, hi) ->
              putStrLn $
                "Диапазон высот (индексы полутонов): "
                  ++ show lo ++ " .. " ++ show hi

analyzeSpotifyLike :: Track -> String -> IO ()
analyzeSpotifyLike t url = do
  let s        = title t ++ artist t ++ url
      base     = sum (map ord s)
      tempo    = 60  + base `mod` 100
      energy   = fromIntegral (base `mod` 100) / 100.0
      dance    = fromIntegral ((base `div` 3) `mod` 100) / 100.0
      valence  = fromIntegral ((base `div` 7) `mod` 100) / 100.0
      acoustic = fromIntegral ((base `div` 11) `mod` 100) / 100.0

  putStrLn $ "Название: " ++ title t
  putStrLn $ "Исполнитель: " ++ artist t
  putStrLn $ "Жанр (по нашему каталогу): " ++ show (genre t)
  putStrLn $ "Темп (BPM):      " ++ show tempo
  putStrLn $ "Энергия (0..1):    " ++ show energy
  putStrLn $ "Танцевальность:     " ++ show dance
  putStrLn $ "Позитивность:          " ++ show valence
  putStrLn $ "Акустичность:     " ++ show acoustic
  putStrLn   "------------------------------------"

playTrackIO :: IO ()
playTrackIO = do
  putStrLn "Введите id трека для проигрывания:"
  tid <- readIntFromLine
  case trackAudioPath tid of
    Nothing ->
      putStrLn "Для этого трека нет привязанного аудиофайла или ссылки."
    Just path -> do
      putStrLn $ "Открываю: " ++ path
      playAudioFile path

playAudioFile :: FilePath -> IO ()
playAudioFile path =
  case os of
    "mingw32" -> callCommand $ "start \"\" \"" ++ path ++ "\""  -- Windows!!
    "linux"   -> callCommand $ "xdg-open \"" ++ path ++ "\""    -- Linux
    "darwin"  -> callCommand $ "open \"" ++ path ++ "\""        -- macOS
    _         -> putStrLn "Неизвестная ОС, не могу открыть аудиофайл."


importExternalPlaylistIO :: IO (Maybe ExternalPlaylist)
importExternalPlaylistIO = do
  putStrLn "Введите путь к файлу с ссылками (по одной в строке):"
  path <- getLine
  contentsOrError <- safeReadFile path
  case contentsOrError of
    Left err -> do
      putStrLn $ "Не удалось прочитать файл: " ++ err
      return Nothing
    Right contents -> do
      let ls   = lines contents
          urls = filter (not . null) (map trim ls)
      if null urls
        then do
          putStrLn "В файле не найдено ни одной непустой строки."
          return Nothing
        else do
          let pl =
                ExternalPlaylist
                  { epName = path,
                    epUrls = urls
                  }
          putStrLn $
            "Импортирован плейлист из файла: " ++ path
              ++ " (треков: "
              ++ show (length urls)
              ++ ")"
          return (Just pl)

showExternalPlaylist :: Maybe ExternalPlaylist -> IO ()
showExternalPlaylist Nothing =
  putStrLn "Импортированный плейлист пока не загружен."
showExternalPlaylist (Just ep) = do
  putStrLn $ "Импортированный плейлист: " ++ epName ep
  if null (epUrls ep)
    then putStrLn "Плейлист пуст."
    else do
      putStrLn "Ссылки:"
      mapM_ (\(i,u) -> putStrLn (show i ++ ". " ++ u)) (zip [1 :: Int ..] (epUrls ep))

playFromExternalPlaylistIO :: Maybe ExternalPlaylist -> IO ()
playFromExternalPlaylistIO Nothing =
  putStrLn "Импортированный плейлист пока не загружен."
playFromExternalPlaylistIO (Just ep) =
  if null (epUrls ep)
    then putStrLn "В импортированном плейлисте нет ссылок."
    else do
      putStrLn $ "Импортированный плейлист: " ++ epName ep
      mapM_ (\(i,u) -> putStrLn (show i ++ ". " ++ u)) (zip [1 :: Int ..] (epUrls ep))
      putStrLn "Введите номер трека для проигрывания:"
      idx <- readIntFromLine
      if idx < 1 || idx > length (epUrls ep)
        then putStrLn "Нет трека с таким номером."
        else do
          let url = epUrls ep !! (idx - 1)
          putStrLn $ "Открываю: " ++ url
          playAudioFile url

analyzeFromExternalPlaylistIO :: Maybe ExternalPlaylist -> IO ()
analyzeFromExternalPlaylistIO Nothing =
  putStrLn "Импортированный плейлист пока не загружен."
analyzeFromExternalPlaylistIO (Just ep) =
  if null (epUrls ep)
    then putStrLn "В импортированном плейлисте нет ссылок."
    else do
      putStrLn $ "Импортированный плейлист: " ++ epName ep
      mapM_ (\(i,u) -> putStrLn (show i ++ ". " ++ u)) (zip [1 :: Int ..] (epUrls ep))
      putStrLn "Введите номер ссылки для анализа:"
      idx <- readIntFromLine
      if idx < 1 || idx > length (epUrls ep)
        then putStrLn "Нет трека с таким номером."
        else do
          let url = epUrls ep !! (idx - 1)
          analyzeExternalSpotifyUrl url

analyzeExternalSpotifyUrl :: String -> IO ()
analyzeExternalSpotifyUrl url = do
  let s        = url
      base     = sum (map ord s)
      tempo    = 60  + base `mod` 100
      energy   = fromIntegral (base `mod` 100) / 100.0
      dance    = fromIntegral ((base `div` 3) `mod` 100) / 100.0
      valence  = fromIntegral ((base `div` 7) `mod` 100) / 100.0
      acoustic = fromIntegral ((base `div` 11) `mod` 100) / 100.0

  putStrLn $ "Ссылка: " ++ url
  putStrLn $ "Темп (BPM):      " ++ show tempo
  putStrLn $ "Энергия (0..1):  " ++ show energy
  putStrLn $ "Танцевальность:  " ++ show dance
  putStrLn $ "Позитивность:    " ++ show valence
  putStrLn $ "Акустичность:    " ++ show acoustic
  putStrLn "-----------------------------------------------------"

readIntFromLine :: IO Int
readIntFromLine = do
  s <- getLine
  case reads s of
    [(n, "")] -> return n
    _ -> do
      putStrLn "Инвалид намбер мяу:"
      readIntFromLine

waitForEnter :: IO ()
waitForEnter = do
  putStrLn ""
  putStrLn "Нажми Enter, чтобы вернуться в меню..........."
  _ <- getLine
  return ()

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  res <- try (readFile path) :: IO (Either IOException String)
  case res of
    Left e  -> return (Left (show e))
    Right t -> return (Right t)

searchAndSaveSpotifyLinkIO :: IO ()
searchAndSaveSpotifyLinkIO = do
  putStrLn "Название исполнителя ппзл:"
  q <- getLine
  putStrLn "а куда сохранить? лучше бери на playlist.txt"
  out <- getLine

  hits <- searchTracks q
  if null hits
    then putStrLn "Ничего не нашла :("
    else do
      putStrLn "Теоретически возможно нашлось:"
      mapM_ (\(i,h) ->
               putStrLn $
                 show i ++ ". " ++ hitName h ++ " - " ++ hitArtist h
                 ++ "\n    " ++ trackHitUrl h
            ) (zip [1 :: Int ..] hits)

      putStrLn "Введите цифру трека, который сохранить:"
      idx <- readIntFromLine
      if idx < 1 || idx > length hits
        then putStrLn "Нет такой циферки"
        else do
          let url = trackHitUrl (hits !! (idx - 1))
          appendFile out (url ++ "\n")
          putStrLn $ "Сохранила ссылку в файл: " ++ out
          putStrLn $ "Вероятно добавлено: " ++ url