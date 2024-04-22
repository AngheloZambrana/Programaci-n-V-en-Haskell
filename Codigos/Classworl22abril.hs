module Classworl22abril where
import Control.Applicative (Alternative (empty))
import Graphics.UI.Threepenny (emphasize)
import GHC.Base (Alternative((<|>)))
    
-- Definición del tipo de datos MyMaybe
data MyMaybe a = MyNothing | MyJust a deriving (Show)

-- Instancia de Functor para MyMaybe
instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

-- Instancia de Applicative para MyMaybe
instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> something = fmap f something

-- Instancia de Alternative para MyMaybe
instance Alternative MyMaybe where
    empty = MyNothing
    MyNothing <|> m = m
    MyJust m <|> _ = MyJust m


-- Función de suma segura que maneja MyMaybe
suma :: MyMaybe Int -> MyMaybe Int -> MyMaybe Int
suma MyNothing _ = MyNothing  
suma _ MyNothing = MyNothing  
suma (MyJust x) (MyJust y) = MyJust (x + y)  

main :: IO ()
main = do
    print $ suma (MyJust 5) (MyJust 3)  
    print $ suma MyNothing (MyJust 3)  
    print $ suma (MyJust 5) MyNothing   
    print $ suma MyNothing MyNothing    
