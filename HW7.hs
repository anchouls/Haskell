module HW where


import Data.Complex
import Data.Char
import Data.Monoid
import Data.List
import Data.Foldable
import Control.Applicative

{-
Сделайте тип из предыдущего домашнего задания представителем классов типов Foldable и Traversable
-}

data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
  foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))
  foldl f ini (Tr x y z) = ini `f` x `f` y `f` z

instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z
  sequenceA (Tr a b c) = Tr <$> a <*> b <*> c


{-
Сделайте двоичное дерево представителем класса типов Traversable
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

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch l x r) = pure Branch <*> traverse f l <*> f x <*> traverse f r



{-
Сделайте тип представителем класса типов Foldable
-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g)  where
  foldMap f (Cmps x) = foldMap (foldMap f) x


{-
Сделайте тип представителем класса типов Traversable.
-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap f (Cmps x) = Cmps (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Cmps f g)  where
    foldMap f (Cmps x) = foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable (Cmps f g)  where
    traverse f (Cmps x) = pure Cmps <*> traverse (traverse f) x

{-
Сделайте приведенный выше парсер представителем Applicative и Alternative, так чтобы обеспечить поведение, согласованное с возможностью неоднозначного разбора
-}

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(f a, s1) | (a, s1) <- p s])

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  u <*> v = Parser $ \s -> [(f a, s2) | (f, s1) <- apply u s, (a, s2) <- apply v s1]

instance Alternative Parser where
  empty = Parser (const [])
  u <|> v = Parser (\s -> apply u s ++ apply v s)

{-
Напишите представителей моноидального функтора для Maybe, пары и ((->) e)
-}

class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  Nothing *&* _ = Nothing
  _ *&* Nothing = Nothing
  (Just a) *&* (Just b) = Just (a,b)

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (f,a) *&* (g,b) = (mappend f g, (a,b))

instance Monoidal ((->) e) where
  unit = pure mempty
  f *&* g = pure (,) <*> f <*> g


{-
Покажите, что всякий аппликативный функтор моноидален. Для этого реализуйте функции
-}

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' a b = pure (,) <*> a <*> b

{-
Покажите, что всякий моноидальный функтор аппликативен. Для этого реализуйте функции
-}

pure' :: Monoidal f => a -> f a
pure' a = fmap (const a) unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = fmap (uncurry ($)) $ a *&* b
