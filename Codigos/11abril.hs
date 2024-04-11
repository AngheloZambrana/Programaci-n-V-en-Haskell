module Apuntes where

import Control.Applicative

data CMaybe a = CNothing | CJust Int a deriving(Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- fmap (++ "ha") (CJust 0 "ho")
-- fmap (++ "he") (fmap (++ "ha") (CJust 0 "ho"))
-- fmap (++ "blah") CNothing

maybeAdd :: Maybe (Int -> Int -> Int)
maybeAdd = pure (+)

maybeInt1 :: Maybe Int
maybeInt1 = Just 5

maybeInt2 :: Maybe Int
maybeInt2 = Just 7

result :: Maybe Int
result = maybeAdd <*> maybeInt1 <*> maybeInt2

addToList :: [Int -> Int -> Int]
addToList = [(+), (-)]

result2 :: [Int]
result2 = addToList <*> [1,2] <*> [1]