module Prob2
  ( recursionpik,
    tailrecpik,
    modulpik,
    mappedpik,
    infinitypik
  )
where

import Data.List (maximumBy)
import Data.Function (on)

naturalNumbers :: [Integer]
naturalNumbers = [1 ..]

normalize :: Integer -> Integer
normalize n
  | n `mod` 2 == 0 = normalize (n `div` 2)
  | n `mod` 5 == 0 = normalize (n `div` 5)
  | otherwise = n

recursionpik :: Integer -> Integer
recursionpik n = search 1
  where
    m = normalize n
    search k
      | (10 ^ k - 1) `mod` m == 0 = k
      | otherwise = search (k + 1)

tailrecpik :: Integer -> Integer
tailrecpik n = go 1
  where
    m = normalize n
    go k
      | (10 ^ k - 1) `mod` m == 0 = k
      | otherwise = go (k + 1)

modulpik :: Integer -> Integer
modulpik n = head (filter (\x -> (10 ^ x - 1) `mod` m == 0) [1 ..])
  where
    m = normalize n

mappedpik :: Integer -> Integer
mappedpik n = fst . head . filter snd $
  map (\x -> (x, (10 ^ x - 1) `mod` m == 0)) [1 ..]
  where
    m = normalize n

infinitypik :: Integer -> Integer
infinitypik n = head [x | x <- naturalNumbers, (10 ^ x - 1) `mod` m == 0]
  where
    m = normalize n