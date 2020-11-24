module HW where


import Data.Complex
import Data.Char
import Data.Monoid
import Data.List
import Data.Foldable

{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон.
-}

revRange :: (Char, Char) -> [Char]
revRange (a, b) = unfoldr fun (a, b)
fun = \(x,y) -> if ord(y) < ord(x) then Nothing else Just (y, (x, chr (ord y - 1)))


{-
Напишите реализации функций из стандартной библиотеки tails, inits :: [a] -> [[a]] через свёртку foldr
-}

tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun = \x xs -> (x : head xs):xs
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' = \x xs -> []: map (x:) xs
ini' = [[]]

{-
Напишите две реализации функции обращения списка reverse :: [a] -> [a] через свёртки foldr и foldl
-}

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' = \x xs -> xs ++ [x]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' = \xs x -> x:xs
ini'' = []


{-
Напишите реализацию оператора "безопасного" поиска элемента списка по индексу (!!!) :: [a] -> Int -> Maybe a через foldr:
-}

infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n
fun x g n | n < 0  = Nothing
          | n == 0 = Just x
          | n > 0  = g (n - 1)
ini = const Nothing

{-
Напишите реализацию foldl через foldr
-}

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
fun f = \x xs v -> xs (v `f` x)
ini = id

{-
Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева.
Сделайте двоичное дерево представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal).
Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), упаковав дерево в типы-обертки и сделав эти обертки представителями класса Foldable.
-}


data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)


instance Foldable Tree where
    foldMap f Nil = mempty
    foldMap f (Branch l x r) = (foldMap f l) <> f x <> (foldMap f r)

instance Foldable Preorder where
    foldMap f (PreO Nil) = mempty
    foldMap f (PreO (Branch l x r)) = f x <> (foldMap f (PreO l)) <> (foldMap f (PreO r))

instance Foldable Postorder where
    foldMap f (PostO Nil) = mempty
    foldMap f (PostO (Branch l x r)) = (foldMap f (PostO l)) <> (foldMap f (PostO r)) <> f x

instance Foldable Levelorder where
    foldMap f (LevelO Nil) = mempty
    foldMap f (LevelO tree)  = foldl1 (<>) (map (f . root) (concat (unfoldr helper [tree])))
           where
                helper x = if (null x) then Nothing else Just (x, concatMap leaf x)
                root (Branch l x r) = x
                leaf (Branch l x r) = filter (\x -> case x of
                                                    Nil -> False
                                                    _ -> True) [l,r]
