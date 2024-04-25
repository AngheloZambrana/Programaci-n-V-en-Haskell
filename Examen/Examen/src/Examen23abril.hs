module Examen23abril where

import Control.Applicative

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Alternative Pair where
    empty = Pair undefined undefined
    Pair x1 y1 <|> Pair x2 y2 = Pair x1 y2

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Applicative Tree where
    pure x = Leaf x
    Leaf f <*> t = fmap f t
    Node f left1 right1 <*> Node x left2 right2 = Node (f x) (left1 <*> left2) (right1 <*> right2)


instance Alternative Tree where
    empty = Leaf undefined
    Leaf x <|> tree = tree
    Node x left1 right1 <|>  empty = Node x left1 right1
    Node x1 left1 right1 <|> Node x2 left2 right2 = Node x1 (left1 <|> left2) (right1 <|> right2)

incrementar :: Tree Int -> Tree Int    --Me di cuenta que estaba mal implementa con el (+1) que hice en mi hoja
incrementar (Leaf x) = Leaf (x + 1)
incrementar (Node x left right) = Node (x + 1) (incrementar left) (incrementar right)

convertir :: Tree String -> Tree Int --Mal implementado el lenguth en mi hoja
convertir (Leaf "x") = Leaf 1
convertir (Leaf s) = Leaf (length s)
convertir (Node x left right) = Node (length x) (convertir left) (convertir right)

sumarArboles :: Tree Int -> Tree Int -> Tree Int
sumarArboles (Leaf x) (Leaf y) = Leaf (x + y)
sumarArboles (Node x1 left1 right1) (Node x2 left2 right2) = Node (x1 + x2) (sumarArboles left1 left2) (sumarArboles right1 right2)

concatenarArboles :: [Tree a] -> Tree a
concatenarArboles [] = Leaf undefined
concatenarArboles (Leaf x : xs) = Node x (Leaf x) (concatenarArboles xs)
concatenarArboles (Node x left right : xs) = Node x left (right <|> concatenarArboles xs)
