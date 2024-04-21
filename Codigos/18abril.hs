module Codigos where
import Control.Applicative

--instance Applicative [] where
    -- pure :: a -> [a]
   -- pure x = [x]
    -- (<*>) :: [a -> b] -> [a] -> [b]
   -- gs <*> xs = [g x | g <- gs, x <- xs]

zipList1 :: ZipList Int
zipList1 = ZipList [1,2,3]

zipList2 :: ZipList Int
zipList2 = ZipList [4,5,6]

result :: ZipList Int
result = (*) <$> zipList1 <*> zipList2

getChar2 :: Int -> IO String
getChar2 0 = return []
getChar2 n = pure (:) <*> getChar <*> getChar2 (n - 1)

