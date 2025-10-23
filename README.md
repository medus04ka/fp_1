# Лабораторная работа 2.
Вариант: Bag (мульти-множество на хеш-таблице)

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## ye b [eq c ybv Реализацией] 

## Структура 
Мешок (`Bag`) реализован как враппер над собственной хеш-таблицей `Table`, позволяющей хранить количество вхождений каждого элемента:
```
newtype Bag a = Bag (SCH.Table a Int)
    deriving (Show)
```
Каждый ключ (`a`) сопоставлен с количеством его появлений в коллекции (`Int`) \

Реализация хеш-таблицы (`Table`) находится в модуле `SCHashmap` и поддерживает стандартные операции:

- insert, insertWith
- lookup, member
- delete
- unionWith
- toList

## Основные операции Bag
### Вставка элементов
```
insert :: (Eq a, Hashable a) => a -> Bag a -> Bag a
insert x (Bag table) = Bag (SCH.insertWith (+) x 1 table)
```
### Удаление элементов
```
delete :: (Eq a, Hashable a) => a -> Bag a -> Bag a
delete x (Bag table) =
    case SCH.lookup x table of
        Just n | n > 1 -> Bag (SCH.insert x (n - 1) table)
        _ -> Bag (SCH.delete x table)
```
### Объединение (юнионы)
```
union :: (Eq a, Hashable a) => Bag a -> Bag a -> Bag a
union (Bag t1) (Bag t2) = Bag (SCH.unionWith (+) t1 t2)
```
### Разность и пересечение
```
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
```
### Реализация классов типов (Monoid и Semigroup)
```
instance (Eq a, Hashable a) => Semigroup (Bag a) where
    (<>) = union

instance (Eq a, Hashable a) => Monoid (Bag a) where
    mempty = empty
```
### Eq
Для корректного сравнения `Bag` (независимого от порядка бакетов):
```
instance (Eq a, Hashable a, Ord a) => Eq (Bag a) where
    (Bag a) == (Bag b) = sortOn fst (SCH.toList a) == sortOn fst (SCH.toList b)
```
### Тестирование
Тестирование выполнено с помощью библиотеки QuickCheck \
Проверяются свойства моноида и корректность вычисления размера
```
main :: IO ()
main = do
  putStrLn "Testing Monoid)()()() ..."
  quickCheck (\(a :: B.Bag Int) -> B.propMonoidLeftId a)
  quickCheck (\(a :: B.Bag Int) -> B.propMonoidRightId a)
  quickCheck (\(a :: B.Bag Int) (b :: B.Bag Int) (c :: B.Bag Int) ->
                B.propMonoidAssociativity a b c)

  putStrLn "Testing size correctness............."
  quickCheck (prop_sizeCorrectness :: [Int] -> Bool)
  ```

  ### Результаты тестирования
```
  Testing Monoid)()()() ...
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
Testing size correctness.............
+++ OK, passed 100 tests.

soska> Test suite soska-test passed
```
# Вывод
Очень убило дебагами, но интерес все таки был, правда под конец я уже плакала, примерно также как и делала лабу по аллокатору, то там я схитрила, а тут честное пионерское (надеюсь) в общем, лаба крутая, мои нервы нет, но послушав про лейзи и черно красное, то задумывается о том, что у меня все же легче \
![alt text](imgggg.jpg)