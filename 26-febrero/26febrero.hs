module Classwork where
    cadena :: String -> [String]
    cadena "" = []
    cadena a = [a]

    --x = "Hola Mundo", y = [("Hola"), ("Mundo")]
    split :: String -> Char -> [String]
    split [] _ = []
    split (x:xs) c
        | x == c = split xs c
    --    | otherwise = [x] : split xs c
        