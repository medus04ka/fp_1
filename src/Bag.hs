module Bag (
    Bag,
    empty,
    singleton,
    fromList,
    toList,
    insert,
    insertList,
    delete,
    deleteList,
    union,
    difference,
    intersection,
    member,
    notMember,
    size,
    filterBag,
    propMonoidLeftId,
    propMonoidRightId,
    propMonoidAssociativity
) where

import qualified SCHashmap as SCH
import Data.List (intersect, sortOn)
import Data.Hashable

newtype Bag a = Bag (SCH.Table a Int)
    deriving (Show)

empty :: Bag a
empty = Bag SCH.empty

singleton :: (Eq a, Hashable a) => a -> Bag a
singleton x = Bag (SCH.insert x 1 SCH.empty)

fromList :: (Eq a, Hashable a) => [a] -> Bag a
fromList = insertList empty

toList :: Bag a -> [a]
toList (Bag table) =
    concatMap (\(k, n) -> replicate n k) (SCH.toList table)


insert :: (Eq a, Hashable a) => a -> Bag a -> Bag a
insert x (Bag table) = Bag (SCH.insertWith (+) x 1 table)

insertList :: (Eq a, Hashable a) => Bag a -> [a] -> Bag a
insertList = foldr insert

delete :: (Eq a, Hashable a) => a -> Bag a -> Bag a
delete x (Bag table) =
    case SCH.lookup x table of
        Just n | n > 1 -> Bag (SCH.insert x (n - 1) table)
        _ -> Bag (SCH.delete x table)

deleteList :: (Eq a, Hashable a) => Bag a -> [a] -> Bag a
deleteList = foldl (flip delete)


member :: (Eq a, Hashable a) => a -> Bag a -> Bool
member x (Bag table) = SCH.member x table

notMember :: (Eq a, Hashable a) => a -> Bag a -> Bool
notMember x bag = not (member x bag)


union :: (Eq a, Hashable a) => Bag a -> Bag a -> Bag a
union (Bag t1) (Bag t2) = Bag (SCH.unionWith (+) t1 t2)

difference :: (Eq a, Hashable a) => Bag a -> Bag a -> Bag a
difference (Bag t1) (Bag t2) =
    Bag $ foldr update t1 (SCH.toList t2)
  where
    update (k, n) acc =
        case SCH.lookup k acc of
            Just m | m > n -> SCH.insert k (m - n) acc
            _              -> SCH.delete k acc


intersection :: (Eq a, Hashable a) => Bag a -> Bag a -> Bag a
intersection (Bag t1) (Bag t2) =
    Bag $ foldr addIfCommon SCH.empty (SCH.toList t1)
  where
    addIfCommon (k, n1) acc =
        case SCH.lookup k t2 of
            Just n2 -> SCH.insert k (min n1 n2) acc
            Nothing -> acc

size :: Bag a -> Int
size (Bag table) = sum (map snd (SCH.toList table))


filterBag :: (Eq a, Hashable a) => (a -> Bool) -> Bag a -> Bag a
filterBag p bag =
    fromList (filter p (toList bag))

propMonoidLeftId :: (Eq a, Ord a, Hashable a) => Bag a -> Bool
propMonoidLeftId a = empty `union` a == a

propMonoidRightId :: (Eq a, Ord a, Hashable a) => Bag a -> Bool
propMonoidRightId a = a `union` empty == a

propMonoidAssociativity :: (Eq a, Ord a, Hashable a) => Bag a -> Bag a -> Bag a -> Bool
propMonoidAssociativity a b c =
    a `union` (b `union` c) == (a `union` b) `union` c

instance (Eq a, Hashable a) => Semigroup (Bag a) where
    (<>) = union

instance (Eq a, Hashable a) => Monoid (Bag a) where
    mempty = empty

instance (Eq a, Hashable a, Ord a) => Eq (Bag a) where
    (Bag a) == (Bag b) =
        sortOn fst (SCH.toList a) == sortOn fst (SCH.toList b)