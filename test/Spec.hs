import Data.Text as T
import Data.Text.IO as TIO
import qualified Prob2 as K
import qualified Prob1 as R

main :: IO ()
main = do
  TIO.putStrLn "1 ntcn ydf[e]"
  assertEquals (R.tailrecpic 20, 232792560) "Хвостовая рекурсия"
  assertEquals (R.recursionpic 20, 232792560) "Рекурсия"
  assertEquals (R.modulpic 20, 232792560) "Модули"
  assertEquals (R.mappedpic 20, 232792560) "Мапа"
  assertEquals (R.infinitypic 20, 232792560) "бессконечности"
  TIO.putStrLn "end\n"

  TIO.putStrLn "2 ghj,ktvd"
  assertEquals (K.tailrecpik 3, 1) "Хвостовая рекурсия"
  assertEquals (K.recursionpik 1000, 1) "Рекурсия"
  assertEquals (K.modulpik 7, 6) "Модули"
  assertEquals (K.mappedpik 999, 3) "Мапа"
  assertEquals (K.infinitypik 77, 6) "бессконечности"
  TIO.putStrLn "end\n"

assertEquals :: (Eq a) => (a, a) -> T.Text -> IO ()
assertEquals (a, b) testDesc =
  TIO.putStrLn $ (if a == b then "Passed: " else "Failed: ") <> testDesc