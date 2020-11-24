module HW where


import Data.List

{-
Реализуйте функцию cmp, сравнивающую элементы типа LogLevel так, чтобы имел место порядок Error > Warning > Info
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Info _ = LT
cmp _ Error = LT
cmp _ Info = GT

{-
Реализуйте функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть если имя было "John", то после применения этой функции, оно превратится в "J.".
Однако если имя было короче двух символов, то оно не меняется.
-}

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = name (firstName p) }
    where
        name p = if length p <= 2
                 then p
                 else (take 1 p) ++ "."

{-
Напишите функции, которые вычисляют сумму элементов дерева и максимальную высоту дерева:
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)

treeSum :: Tree Integer -> Integer
treeSum (Leaf) = 0
treeSum (Node l a r) = a + treeSum l + treeSum r

treeHeight :: Tree a -> Int
treeHeight (Leaf) = 0
treeHeight (Node l a r) = 1 + max (treeHeight l) (treeHeight r)



{-
Составить список сумм соответствующих элементов трех заданных списков.
Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом "закончившиеся" списки не должны давать вклада в суммы.
-}

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as [] [] = as
sum3 [] bs [] = bs
sum3 [] [] cs = cs
sum3 (a:as) (b:bs) [] = (a + b):sum3 as bs []
sum3 (a:as) [] (c:cs) = (a + c):sum3 as [] cs
sum3 [] (b:bs) (c:cs) = (b + c):sum3 [] bs cs
sum3 (a:as) (b:bs) (c:cs) = (a + b + c):sum3 as bs cs


{-
Сформируйте список цифр заданного целого числа.
-}

digits :: Integer -> [Integer]
digits x = if abs(x) `div` 10 == 0
           then [abs(x)]
           else (digits (abs(x) `div` 10)) ++ [abs(x) `mod` 10]




{-
Определите, содержит ли заданное целое число все цифры от 1 до 9. (Используйте функцию digits из предыдущего задания.)
-}

containsAllDigits :: Integer -> Bool
containsAllDigits x = helper x 1
      where helper x n = if n == 10
                                  then True
                                  else if elem n (digits x)
                                       then helper x (n+1)
                                       else False
                                             where
                                                 elem x [] = False
                                                 elem x (y : ys) = if x == y
                                                                   then True
                                                                   else elem x ys

{-
Определите, содержит ли заданное целое число все цифры от 1 до 9 в точности по одному разу. (Используйте функцию digits из предыдущего задания.)
-}

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x = helper x 1
      where helper x n = if n == 10
                         then True
                         else if elem n (digits x)
                              then helper x (n+1)
                              else False
                                   where
                                       elem x [] = False
                                       elem x (y : ys) = if x == y
                                                         then notElem x ys
                                                         else elem x ys
                                                            where notElem x [] = True
                                                                  notElem x (y : ys) = if x == y
                                                                             then False
                                                                             else notElem x ys

{-
Из заданного списка выделите подсписок, состоящий из элементов с nn-го номера по kk-ый, считая от нуля. При этом nn-ый элемент должен входить в результат, а kk-ый - нет.
-}

sublist :: Int -> Int -> [a] -> [a]
sublist n k (x:xs)
              | n < 0 = sublist 0 k (x:xs)
              | k < 0 = []
              | otherwise = take (k-n) (drop n (x:xs))

{-
Повторите каждый элемент списка заданное число раз.
-}

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n [] = []
repeatEveryElem n (x:xs) = (take n (repeat x)) ++ (repeatEveryElem n xs)


{-
Дан список (возможно бесконечный) и положительное целое число n.
Создайте спиcок "скользящих" подсписков длины n.
-}

movingLists :: Int -> [a] -> [[a]]
movingLists n x = zipWith (const . take n) (tails x) (drop (n-1) x)
