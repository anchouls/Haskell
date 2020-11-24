module HW where


import Data.List
import Data.Complex

{-
Сделайте тип Matrix a = Matrix [[a]] представителем класса типов Show.
Строки матрицы (внутренние списки) должны изображаться как списки; каждый следующий внутренний список должен начинаться с новой строки (используйте символ'\n' в качестве разделителя).
Пустая матрица должна выводиться как EMPTY.
-}

newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
            show (Matrix []) = "EMPTY"
            show (Matrix [x]) = show(x)
            show (Matrix (x:xs)) = show(x) ++ "\n" ++ show (Matrix xs)


{-
Сделайте тип-обертку представителем классов типов Show и Read.
Представитель класса типов Show должен использовать разделители вещественной и мнимой части +i* и -i*, зависящие от знака мнимой части:
Представитель класса типов Read должен быть точным дополнением представителя класса Show, то есть для любого z :: Cmplx должно выполняться
-}


newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
    show(Cmplx x) = if imagPart x > 0
                    then show(realPart x) ++ "+i*" ++ show(imagPart x)
                    else show(realPart x) ++ "-i*" ++ show(abs(imagPart x))

newCmplx :: (Complex Double) -> Cmplx
newCmplx x = Cmplx x

instance Read Cmplx where
  readsPrec _ a =
        let [(x,(y:ys))] = (reads a) :: [(Double,[Char])]
            z = if y == '-' then "(-" ++ drop 3 (y:ys) ++ ")" else drop 3 (y:ys)
            o = (read (show x ++ ":+" ++ z)) :: (Complex Double)
        in [(newCmplx o , "")]


{-
Реализуйте класс типов обе функции которого ведут себя как succ и pred стандартного класса Enum, однако являются тотальными, то есть не останавливаются с ошибкой на наибольшем и наименьшем значениях типа-перечисления соответственно, а обеспечивают циклическое поведение. Ваш класс должен быть расширением ряда классов типов стандартной библиотеки, так чтобы можно было написать реализацию по умолчанию его методов, позволяющую объявлять его представителей без необходимости писать какой бы то ни было код.
-}

class (Enum a, Bounded a, Ord a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if x == maxBound
            then minBound
            else succ x

  spred :: a -> a
  spred x = if x == minBound
            then maxBound
            else pred x


{-
Реализуйте функцию, задающую циклическое вращение списка.
-}

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n xs = if n > 0
              then zipWith const (drop n $ cycle xs) xs
              else zipWith const (drop ((n `rem` (length xs)) + (length xs)) $ cycle xs) xs


{-
Найдите все сочетания по заданному числу элементов из заданного списка.
-}

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb n (x:xs) = [ x:ys | ys <- comb (n-1) xs ] ++ comb n xs
