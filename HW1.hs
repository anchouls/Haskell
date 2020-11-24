module HW where


{-
Определите функцию вычисляющую двойной факториал, то есть произведение натуральных чисел, не превосходящих заданного числа и имеющих ту же четность.
Например:7!!=7⋅5⋅3⋅1,8!!=8⋅6⋅4⋅2.
-}

doubleFact :: Integer -> Integer
doubleFact n = helper 1 n
               where
                 helper :: Integer -> Integer -> Integer
                 helper acc n = if n > 1
                                    then helper (acc * n) (n - 2)
                                    else acc
{-
Реализуйте функцию, находящую элементы следующей рекуррентной последовательности
a0=1;a1=2;a2=3;a(k+3)=a(k+2)+a(k+1)−2a(k).
-}

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = helper 1 2 3 3 n
  where
    helper :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
    helper k k1 k2 i n = if i == n
                            then k3
                            else helper k1 k2 k3 (i + 1) n
                                where k3 = k2 + k1 - 2*k

{-
Понятие чисел Фибоначчи можно расширить, потребовав, чтобы рекуррентное соотношение выполнялось для произвольных целых значений аргумента, в том числе и отрицательных.
Реализуйте функцию, вычисляющую числа Фибоначчи так, чтобы она удовлетворяла этому требованию.
-}

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci (-2) = (-1)
fibonacci n = if n > 0
              then helper 0 1 2 n
              else ((-1) ^ (1-n)) * fibonacci (-n)
    where
        helper :: Integer -> Integer -> Integer -> Integer -> Integer
        helper f1 f2 i n = if i == n
                           then f1 + f2
                           else helper f2 (f1 + f2) (i + 1) n



{-
Реализуйте функцию, находящую сумму и количество цифр заданного целого числа.
-}

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0,1)
sum'n'count n = if n > 0
                then helper 0 0 n
                else helper 0 0 (-n)
      where
          helper :: Integer -> Integer -> Integer -> (Integer, Integer)
          helper sum cnt n = if n == 0
                             then (sum, cnt)
                             else helper (sum + n `mod` 10) (cnt + 1) (n `div` 10)


{-
Реализуйте функцию, находящую значение определённого интеграла от заданной функции на заданном интервале методом трапеций.
-}

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper f a b 1000
                    where
                        helper :: (Double -> Double) -> Double -> Double -> Double -> Double
                        helper f a b n = if n == 0
                                         then 0
                                         else acc + helper f (a + ((b - a) / n)) b (n - 1)
                                                    where acc = ((b - a) / n) * (f (a) + f (a + ((b - a) / n))) / 2
