# Лабораторная работа 3.

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## ye b [eq c ybv Реализацией] 

## Линейная интерполяция 
```
linInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
linInter p l = fst $ help (const 0) p l
  where
    help :: (Fractional a, Ord a, Show a) => Func a -> [Point a] -> [a] -> ([a], Func a)
    help f points list = case points of
      [] -> (map f list, f)
      [(x1, y1)] ->
        let newF x = if f x1 /= y1 then y1 else f x in (map newF list, newF)
      ((x1, y1) : (x2, y2) : _) -> case list of
        [] -> ([], f)
        (cur : gen)
          | cur < x1 ->
              let newF = if f x1 /= y1 then linearF else f
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur >= x1 && cur <= x2 ->
              let newF x
                    | x >= x1 = linearF x
                    | otherwise = f x
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur > x2 ->
              let (rest, finalF) = help f (tail points) gen
               in (f cur : rest, finalF)
        where
          linearF x = k * x + b
          k = (y1 - y2) / (x1 - x2)
          b = y1 - k * x1

```

## Метод Лагранжа
```
lagInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
lagInter = help []
  where
    help :: (Fractional a, Ord a, Show a) => [Point a] -> [Point a] -> [a] -> [a]
    help _ _ [] = []
    help acc points gen@(curr : restGen) = case points of
      [] -> map predict gen
      (point@(x, y) : restP) -> case compare curr x of
        LT -> predict curr : help acc points restGen
        EQ -> y : help acc points restGen
        GT -> help (point : acc) restP gen
      where
        ps = if null points then acc else head points : acc
        predict a = sum (map (calcCoef a) ps)
        xs = map fst ps

        calcCoef t (xi, yi) = yi * foldl (foldWithout t xi) 1 xs / foldl (foldWithout xi xi) 1 xs

        foldWithout x skip accum xi
          | skip == xi = accum
          | otherwise = accum * (x - xi)

```
### Пример использования
```
PS C:\Users\Eblanchek\soska> stack exec soska-exe 0 1 5 0 0
1 2
3 0
(0.0,3.0)
(1.0,2.0)
(2.0,1.0)
(3.0,0.0)
4 10
(4.0,10.0)
ctrl+C
(5.0,20.0)
```
### Результаты тестирования
```
soska> test (suite: soska-test)

Проверка линейной интерполяции бииииииииииииииииииииииииииииииип................
+++ OK, passed 1000 tests.
Проверка Лагранжа юип бип бип............
+++ OK, passed 1000 tests.


soska> Test suite soska-test passed
Completed 2 action(s).
```
# Вывод
Довольно интересно работать с ленивыми вычислениями, но, кажется, они обрубают возможность делать основные обрабатывающие функции хвосторекурсивными, из-за чего мне становится страшно за стек :/ Также не очень понятно, зачем в требованиях указана возможность запускать два метода одновременно, если в однопоточной программе вывод значений не сможет перемешаться по определению независимо от лени)(
А так в целом, вот вам девочка, любите и будьте любимы \
![alt text](imjg.jpg)