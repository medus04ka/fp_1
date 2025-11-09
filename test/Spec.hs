import qualified Data.List as L
import Huy
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Проверка линейной интерполяции бииииииииииииииииииииииииииииииип................"
  quickCheck (withMaxSuccess 1000 checkLinear)
  putStrLn "Проверка Лагранжа юип бип бип............"
  quickCheck (withMaxSuccess 1000 checkLagrange)

checkLinear :: [Double] -> [Double] -> Bool
checkLinear xs ys =
  all (\(a, b) -> abs (a - b) < eps) (zip computed expected)
  where
    uniquePairs = L.nubBy (\(x1, _) (x2, _) -> x1 == x2) (zip (L.sort xs) ys)
    (xVals, expected) = unzip uniquePairs
    computed = linearInterp uniquePairs xVals
    eps = 1e-5

checkLagrange :: [Double] -> [Double] -> Bool
checkLagrange xs ys =
  all (\(a, b) -> abs (a - b) < eps) (zip computed expected)
  where
    cleanPairs = L.nubBy (\(x1, _) (x2, _) -> x1 == x2) (zip (L.sort xs) ys)
    (xList, expected) = unzip cleanPairs
    computed = lagrangeInterp cleanPairs xList
    eps = 1e-5
