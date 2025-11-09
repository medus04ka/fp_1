module Main (main) where

import Graphics.Gnuplot.Simple
import Huy
import System.Environment (getArgs)

readPoints :: Bool -> IO [(Double, Double)]
readPoints dbg = do
  raw <-
    if dbg
      then readFile "testik.txt"
      else getContents
  let toPair line =
        case words line of
          [sx, sy] -> (read sx, read sy)
          _        -> error "Некорректная строка с точкой"
  return $ map toPair (lines raw)

printPairs :: (Show a, Show b) => [(a, b)] -> IO ()
printPairs = mapM_ print
-- 0 -> только линейная
-- 1 -> только лагранж
-- 2 -> обе
pickMethods :: Double -> [[(Double, Double)] -> [Double] -> [Double]]
pickMethods mVal =
  case round mVal of
    0 -> [linearInterp]
    1 -> [lagrangeInterp]
    _ -> [linearInterp, lagrangeInterp]

main :: IO ()
main = do
  args <- getArgs
  let [start, step, stop, mCode, dbgFlag] = map read args :: [Double]
      debug = dbgFlag == 1

  pts <- readPoints debug

  let xs = [start, start + step .. stop]

  let methods = pickMethods mCode

  let results = map (\f -> f pts xs) methods

  if not debug
    then
      mapM_ (printPairs . zip xs) results
    else do
      let plots = map (zip xs) results
      plotPaths
        [ Key Nothing
        , Title "Interpolation demo"
        , Custom "grid" []
        ]
        plots
      _ <- getLine
      return ()