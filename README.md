# Лабораторная работа 3.

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## ye b [eq c ybv Реализацией] 

## Линейная интерполяция 
```
linearInterp :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
linearInterp pts xs = fst $ go (const 0) pts xs
  where
    go :: (Fractional a, Ord a, Show a) => Func a -> [Point a] -> [a] -> ([a], Func a)
    go f ps input = case ps of
      [] -> (map f input, f)
      [(x1, y1)] ->
        let upd x = if f x1 /= y1 then y1 else f x
         in (map upd input, upd)
      ((x1, y1):(x2, y2):rest) ->
        case input of
          [] -> ([], f)
          (x:xs')
            | x < x1 ->
                let newF = if f x1 /= y1 then line else f
                    (ys, lastF) = go newF ps xs'
                 in (newF x : ys, lastF)
            | x >= x1 && x <= x2 ->
                let newF t
                      | t >= x1 = line t
                      | otherwise = f t
                    (ys, lastF) = go newF ps xs'
                 in (newF x : ys, lastF)
            | x > x2 -> go f ((x2, y2):rest) input
        where
          slope = (y1 - y2) / (x1 - x2)
          offset = y1 - slope * x1
          line t = slope * t + offset

```

## Метод Лагранжа
```
lagrangeInterp :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
lagrangeInterp = build []
  where
    build :: (Fractional a, Ord a, Show a) => [Point a] -> [Point a] -> [a] -> [a]
    build _ _ [] = []
    build prev remaining xs@(x:restX) =
      case remaining of
        [] -> map estimate xs
        (p@(px, py):ps) ->
          case compare x px of
            LT -> estimate x : build prev remaining restX
            EQ -> py : build prev remaining restX
            GT -> build (p:prev) ps xs
      where
        curPoints = if null remaining then prev else head remaining : prev
        xsOnly = map fst curPoints

        estimate a = sum [ yi * lagCoeff a xi | (xi, yi) <- curPoints ]

        lagCoeff t xi =
          let num = product [t - xj | xj <- xsOnly, xj /= xi]
              den = product [xi - xj | xj <- xsOnly, xj /= xi]
           in num / den

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
Довольно интересно работать с ленивыми вычислениями, но, кажется, они обрубают возможность делать основные обрабатывающие функции хвосторекурсивными, из-за чего мне становится страшно за стек :/ Остальное все понятно, господин.
А так в целом, вот вам девочка, любите и будьте любимы \
![alt text](imjg.jpg)