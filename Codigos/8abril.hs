module Classwork where

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False


-- Definimos la función identidad
identity :: a -> a
identity x = x

-- Aplicamos fmap con la función identidad
result1 :: Maybe Int
result1 = fmap identity (Just 5)  -- Resultado esperado: Just 5

-- Otro ejemplo
result2 :: Maybe Int
result2 = fmap identity Nothing   -- Resultado esperado: Nothing

--
double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

--
result3 :: Maybe Int
result3 = fmap (double . addOne) (Just 5) --result1 should be Just 12

result4 :: Maybe Int
result4 = (fmap double . fmap addOne) (Just 5) --result Just 12