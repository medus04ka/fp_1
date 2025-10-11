# Лабораторная работа 1. Проект Эйлера
Вариант: 5, 26

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

## Условия задачек

### Задача 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder. What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

### Задача 26
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2 = 0.5 \
1/3 = 0.(3) \
1/4 = 0.25 \
1/5 = 0.2 \
1/6 = 0.1(6) \
1/7 = 0.(142857) \
1/8 = 0.125 \
1/9 = 0.(1) \
1/10 = 0.1 

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle. Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

## ye b [eq c ybv]
### Задачка 5
- Служебная функция `checkI`
```
checkI :: Int -> Int -> Bool
checkI n i = all (\j -> i `mod` j == 0) [1 .. n]
```
- Хвостовая рекурсия:
```
tailrecpic :: Int -> Int
tailrecpic n = go n
  where
    go i
      | checkI n i = i
      | otherwise = go (i + n)
``` 
- Рекурсия (добавлены + и - i, для наглядного примера разницы между хвостовой и обычной)
```
recursionpic :: Int -> Int
recursionpic n = go n
  where
    go i
      | checkI n i = i
      | otherwise = i + go (i + n) - i
```
- Реализация с явным разделением на генерацию, фильтрацию и свертку:
```
modulpic :: Int -> Int
modulpic n = head (valid n (candidates n))
  where
    candidates m = [m, 2*m ..]
    valid m xs = filter (checkI m) xs
```
- Реализация при помощи map:
```
mappedpic :: Int -> Int
mappedpic n =
  fst.head.filter snd $
    map (\x -> (x, checkI n x)) [n, 2*n ..]
```
- Реализация с использованием бесконечных списков:
```
infinitypic :: Int -> Int
infinitypic n = head [i | i <- [n, 2*n ..], checkI n i]
```
- Кусок реализации на питоне для сравнения:
```
for i in range(1,1000):
    fraction = 1/Decimal(i)
    pattern = re.search(r"^[0-9]\.[0-9]*([0-9]{7,}?)(\1+)[0-9]*?$", str(fraction))
    
    length = displaymatch(pattern)

    if length > maxlen:
        maxlen = length
        d = i
```
### Задачка 26
- Служебные функции `naturalNumbers` и `normalize`
```
naturalNumbers :: [Integer]
naturalNumbers = [1 ..]

normalize :: Integer -> Integer
normalize n
  | n `mod` 2 == 0 = normalize (n `div` 2)
  | n `mod` 5 == 0 = normalize (n `div` 5)
  | otherwise = n
```
- Хвостовая рекурсия:
```
tailrecpik :: Integer -> Integer
tailrecpik n = go 1
  where
    m = normalize n
    go k
      | (10 ^ k - 1) `mod` m == 0 = k
      | otherwise = go (k + 1)
```
- Рекурсия
```
recursionpik :: Integer -> Integer
recursionpik n = search 1
  where
    m = normalize n
    search k
      | (10 ^ k - 1) `mod` m == 0 = k
      | otherwise = search (k + 1)
```
- Реализация с явным разделением на генерацию, фильтрацию и свертку:
```
modulpik :: Integer -> Integer
modulpik n = head (filter (\x -> (10 ^ x - 1) `mod` m == 0) [1 ..])
  where
    m = normalize n
```
- Реализация при помощи map:
```
mappedpik :: Integer -> Integer
mappedpik n = fst.head.filter snd $
  map (\x -> (x, (10 ^ x - 1) `mod` m == 0)) [1 ..]
  where
    m = normalize n
```
- Реализация с использованием бесконечных списков:
```
infinitypik :: Integer -> Integer
infinitypik n = head [x | x <- naturalNumbers, (10 ^ x - 1) `mod` m == 0]
  where
    m = normalize n
```
- Кусок реализации на питоне для сравнения:
```
p = [x for x in range(2,top)]

for num in p:
  for idx in range(2,(top//num)+1):
    if num*idx in p:
      p.remove(num*idx)

result = 1;

for i in range(0, len(p)):
    a = math.floor(math.log(divisorMax) / math.log(p[i]));
    result = result * (p[i]**a);
```
# Вывод
Было интересно прочитать книгу "Программирую на Хаскель", прочитала я весь первый модуль, в целом этого достаточно для первой лабы, там и второй так то нужен чуток, но дальше было лень, максимум дочитать до 12 главы! и для первой лабы прям как раз \
![alt text](image.png)