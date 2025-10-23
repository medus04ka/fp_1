module Main (main) where

import Bag

main :: IO ()
main = do
    let b1 :: Bag Int
        b1 = fromList [1, 2, 2, 3, 4, 4, 4]

        b2 :: Bag Int
        b2 = fromList [2, 4, 5]

    putStrLn "бэг 1:"
    print (toList b1)

    putStrLn "бэг 2:"
    print (toList b2)

    putStrLn "Юнион:"
    print (toList $ union b1 b2)

    putStrLn "Intersection:"
    print (toList $ intersection b1 b2)

    putStrLn "Difference:"
    print (toList $ difference b1 b2)

    putStrLn "фильтры (>3):"
    print (toList $ filterBag (>3) b1)