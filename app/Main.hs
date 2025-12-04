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
    demoUser
  )
import Spotify.Model
--TODO: а можно свои треки? переделать\\ 
-- и еще хочется чтоб они песни проигрывались, но это потом\\
-- import Music.Core (Genre (..), Track (..), TrackId, User (..), Like (..), Playlist (..))
-- import Music.EDSL
--чтоб меню не возникало каждый раз после выбора\\а было что типа "назад в меню"

main :: IO ()
main = do
  putStrLn "Welcome to the Haskell Spotify Demo!"
  putStrLn ""
  loop demoLikes demoPlaylist

-- loopсостояние лайков и плейлиста
loop :: [Like] -> Playlist -> IO ()
loop likes pl = do
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
  putStrLn "0. Выйти"
  putStrLn "================"
  putStr "Введите выбор: "
  choice <- getLine
  putStrLn ""
  case choice of
    "1" -> do
      showAllTracks
      loop likes pl
    "2" -> do
      showPlaylist pl
      loop likes pl
    "3" -> do
      pl' <- addTrackToPlaylistIO pl
      loop likes pl'
    "4" -> do
      pl' <- removeTrackFromPlaylistIO pl
      loop likes pl'
    "5" -> do
      likes' <- likeTrackIO likes
      loop likes' pl
    "6" -> do
      showLikedTracks likes
      loop likes pl
    "7" -> do
      showRecommendations likes
      loop likes pl
    "8" -> do
      analyzeTrackIO
      loop likes pl
    "0" -> do
      putStrLn "Пока- пока 🌸"
    _ -> do
      putStrLn "Чета ты набредил, попробуй еще раз. (только без бредика)"
      loop likes pl

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
    [] -> Nothing
    (t : _) -> Just t

--операции с плейлистом
addTrackToPlaylistIO :: Playlist -> IO Playlist
addTrackToPlaylistIO pl = do
  putStrLn "Вводите id трека для добавления:"
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
          putStrLn "Ты уже лайкнул этот трек ЛАЙКАТЬ НЕ НАДО."
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
  let u = demoUser
      likedIds = likesOfUser (userId u) likes
  putStrLn $ "Лайкнутый трек(и?) юзера " ++ userName u ++ ":"
  if null likedIds
    then putStrLn "Пока нет лайкнутых треков. тут желательно лайкнуть что-нибудь"
    else mapM_ printTrackInPlaylist likedIds

showRecommendations :: [Like] -> IO ()
showRecommendations likes = do
  let u = demoUser
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
      let m = musicData t
          beats = totalDuration m
          pr = pitchRange m
      putStrLn $ "Название: " ++ title t
      putStrLn $ "Исполнитель: " ++ artist t
      putStrLn $ "Жанр: " ++ show (genre t)
      putStrLn $ "Общая длительность (в тактах): " ++ show (fromRational beats :: Double)
      case pr of
        Nothing ->
          putStrLn "Диапазон высот: нет нот (только паузы)."
        Just (lo, hi) ->
          putStrLn $
            "Диапазон высот: "
              ++ show lo
              ++ " .. "
              ++ show hi

--чтение Int из строки7
readIntFromLine :: IO Int
readIntFromLine = do
  s <- getLine
  case reads s of
    [(n, "")] -> return n
    _ -> do
      putStrLn "Инвалид намбер мяу:"
      readIntFromLine
