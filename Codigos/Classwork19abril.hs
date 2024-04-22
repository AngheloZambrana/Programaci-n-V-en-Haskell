module Codigos where
import Control.Applicative

-- Definición de MyList
data MyList a = MyEmpty | MyCons a (MyList a) deriving Show

-- Instancia de Functor para MyList
instance Functor MyList where
    fmap _ MyEmpty = MyEmpty
    fmap f (MyCons x xs) = MyCons (f x) (fmap f xs)

-- Instancia de Applicative para MyList
instance Applicative MyList where
    pure x = MyCons x MyEmpty
    MyEmpty <*> _ = MyEmpty
    _ <*> MyEmpty = MyEmpty
    (MyCons f fs) <*> xs = fmap f xs `myAppend` (fs <*> xs)
        where
            myAppend MyEmpty ys = ys
            myAppend (MyCons y ys) zs = MyCons y (myAppend ys zs)

-- Instancia de Alternative para MyList
instance Alternative MyList where
    empty = MyEmpty
    MyEmpty <|> my = my
    my <|> _ = my

-- Función de suma
myFunctionsAdd :: MyList (Int -> Int -> Int)
myFunctionsAdd = MyCons (+) MyEmpty

-- Función de resta
myFunctionsSubs :: MyList (Int -> Int -> Int)
myFunctionsSubs = MyCons (-) MyEmpty

-- Función de multiplicación
myFunctionsMult :: MyList (Int -> Int -> Int)
myFunctionsMult = MyCons (*) MyEmpty

