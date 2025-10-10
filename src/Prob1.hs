module Prob1
  ( tailrecpic,
    recursionpic,
    modulpic,
    mappedpic,
    infinitypic
  )
where

checkI :: Int -> Int -> Bool
checkI n i = all (\j -> i `mod` j == 0) [1 .. n]

tailrecpic :: Int -> Int
tailrecpic n = go n
  where
    go i
      | checkI n i = i
      | otherwise = go (i + n)

recursionpic :: Int -> Int
recursionpic n = go n
  where
    go i
      | checkI n i = i
      | otherwise = i + go (i + n) - i

modulpic :: Int -> Int
modulpic n = head (valid n (candidates n))
  where
    candidates m = [m, 2*m ..]
    valid m xs = filter (checkI m) xs

mappedpic :: Int -> Int
mappedpic n =
  fst.head.filter snd $
    map (\x -> (x, checkI n x)) [n, 2*n ..]

infinitypic :: Int -> Int
infinitypic n = head [i | i <- [n, 2*n ..], checkI n i]