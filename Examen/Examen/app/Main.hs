module Main (main) where

import Examen

main :: IO ()
main = do
    let expr = Or (And (Var 'A') (Var 'B')) (And (Not (Var 'A')) (Var 'C'))
    let assignments = [('A', True), ('B', False), ('C', True)]
    print (evaluate expr assignments)