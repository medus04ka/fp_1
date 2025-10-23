{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck
import qualified Bag as B
import Data.Hashable (Hashable)

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (B.Bag a) where
  arbitrary = B.fromList <$> arbitrary

main :: IO ()
main = do
  putStrLn "Testing Monoid)()()() ..."
  quickCheck (\(a :: B.Bag Int) -> B.propMonoidLeftId a)
  quickCheck (\(a :: B.Bag Int) -> B.propMonoidRightId a)
  quickCheck (\(a :: B.Bag Int) (b :: B.Bag Int) (c :: B.Bag Int) ->
                B.propMonoidAssociativity a b c)

  putStrLn "Testing size correctness............."
  quickCheck (prop_sizeCorrectness :: [Int] -> Bool)

prop_sizeCorrectness :: [Int] -> Bool
prop_sizeCorrectness xs = B.size (B.fromList xs) == length xs