module SCHashmap (
    Table,
    empty,
    insert,
    insertWith,
    delete,
    lookup,
    member,
    toList,
    unionWith
) where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Hashable

data Table k v = Table Int [[(k, v)]]
    deriving (Show)

empty :: Table k v
empty = Table 8 (replicate 8 [])

hashIndex :: (Hashable k) => Int -> k -> Int
hashIndex size key = hash key `mod` size

lookup :: (Eq k, Hashable k) => k -> Table k v -> Maybe v
lookup key (Table size buckets) =
    let bucket = buckets !! hashIndex size key
    in lookup' bucket
  where
    lookup' [] = Nothing
    lookup' ((k,v):xs)
      | k == key = Just v
      | otherwise = lookup' xs

member :: (Eq k, Hashable k) => k -> Table k v -> Bool
member k = maybe False (const True) . lookup k

insert :: (Eq k, Hashable k) => k -> v -> Table k v -> Table k v
insert = insertWith const

insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> Table k v -> Table k v
insertWith f key val (Table size buckets) =
    let i = hashIndex size key
        bucket = buckets !! i
        newBucket = upsert bucket
    in Table size (take i buckets ++ [newBucket] ++ drop (i + 1) buckets)
  where
    upsert [] = [(key, val)]
    upsert ((k,v):xs)
      | k == key  = (k, f v val) : xs
      | otherwise = (k,v) : upsert xs

delete :: (Eq k, Hashable k) => k -> Table k v -> Table k v
delete key (Table size buckets) =
    let i = hashIndex size key
        bucket = filter ((/= key) . fst) (buckets !! i)
    in Table size (take i buckets ++ [bucket] ++ drop (i + 1) buckets)

toList :: Table k v -> [(k, v)]
toList (Table _ buckets) = concat buckets

unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> Table k v -> Table k v -> Table k v
unionWith f t1 t2 = foldr (\(k,v) acc -> insertWith f k v acc) t1 (toList t2)