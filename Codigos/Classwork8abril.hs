module TrabajoClase where

data MyList a = Empty | Node (Maybe a) (MyList a) deriving (Show, Eq)

instance Functor MyList where
  fmap _ Empty        = Empty
  fmap f (Node Nothing xs) = Node Nothing (fmap f xs)
  fmap f (Node (Just x) xs) = Node (Just (f x)) (fmap f xs)

testList :: MyList Int
testList = Node (Just 1) (Node (Just 2) (Node (Just 3) Empty))

plusOne :: Int -> Int
plusOne x = x + 1

timesTwo :: Int -> Int
timesTwo x = x * 2

identityLaw :: MyList Int -> Bool
identityLaw xs = fmap id xs == xs

compositionLaw :: MyList Int -> Bool
compositionLaw xs = fmap (plusOne . timesTwo) xs == fmap (plusOne . timesTwo) xs

main :: IO ()
main = do
  putStrLn "Testing Composition Law:"
  print $ compositionLaw testList
