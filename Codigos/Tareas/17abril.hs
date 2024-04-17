{-# LANGUAGE AllowAmbiguousTypes #-}
module Tarea where
-- 1. Define an instance of the Functor class for the following type of binary trees that have data in
-- their nodes
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving(Show)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

-- 2. Complete the following instance declaration to make the partially-applied function type (a ->)
--  into a functor:
--  Hint: first write down the type of fmap, and then think if you already know a library function that
--  has this type.
    -- instance Functor ((->) r) where
    --    fmap f g = f . g
-- 3. Define an instance of the Applicative class for the type (a ->). If you are familiar with
--   combinatory logic, you might recognise pure and <*> for this type as being the well-known K and
--   S combinators

    -- instance Applicative ((->) a) where
        -- f <*> g = \x -> f x (g x)
        -- pure x _ = x

-- 4. There may be more than one way to make a parameterised type into an applicative functor. For
-- example, the library Control.Applicative provides an alternative ‘zippy’ instance for lists, in
-- which the function pure makes an infinite list of copies of its argument, and the operator <*>
-- applies each argument function to the corresponding argument value at the same position. Complete
-- the following declarations that implement this idea:

--The ZipList wrapper around the list type is required because each type can only have at most one
-- instance declaration for a given class

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    pure x = Z (repeat x)
    (Z gs) <*> (Z xs) = Z (zipWith (\g x -> g x) gs xs)

-- 5. Work out the types for the variables in the four applicative laws.

identityLaw :: (Applicative f, Eq (f a)) => f a -> Bool
identityLaw x = (pure id <*> x) == x

interchangeLaw :: (Applicative f, Eq (f b)) => f (a -> b) -> a -> Bool
interchangeLaw x y = (x <*> pure y) == (pure (\g -> g y) <*> x)

compositionLaw :: (Applicative f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Bool
compositionLaw x y z = (x <*> (y <*> z)) == ((pure (.) <*> x <*> y) <*> z)
