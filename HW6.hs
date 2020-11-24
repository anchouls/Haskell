module HW where


import Data.Complex
import Data.Char
import Data.Monoid
import Data.List
import Data.Foldable
import Control.Applicative (ZipList(ZipList), getZipList)

{-
Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList
-}

a >$< xs = getZipList $ a <$> (ZipList xs)
xs >*< ys = getZipList $ (ZipList xs) <*> (ZipList ys)


{-
Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой, подобной ZipList
-}

data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

{-
Сделайте двоичное дерево функтором и аппликативным функтором, реализовав в последнем случае семантику применения узла к соответствующему узлу второго дерева
-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)


instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure x = Branch (pure x) x (pure x)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Branch fl f fr) <*> (Branch l x r) = Branch (fl <*> l) (f x) (fr <*> r)


{-
Сделайте тип представителем класса типов Applicative.
-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps x) = Cmps (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure (pure x))
  (Cmps f) <*> (Cmps x) = Cmps ((<*>) <$> f <*> x)

{-
Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации
-}

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> (divideList' xs)

{-
Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями классов типов Functor и Applicative
-}

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\e1 e2 -> f (g e1 e2))

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f (g e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\e1 e2 -> x)
  (<*>) (Arr2 f) (Arr2 g) = Arr2 (\e1 e2 -> f e1 e2 (g e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\e1 e2 e3 -> x)
  (<*>) (Arr3 f) (Arr3 g) = Arr3 (\e1 e2 e3 -> f e1 e2 e3 (g e1 e2 e3))
