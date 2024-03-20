module Examen where


data Expr = Var Char 
          | Not Expr
          | And Expr Expr
          | Or  Expr Expr
          deriving(Show)

generateTruthTable :: Expr -> [(Char, Bool, Bool, Bool)]
generateTruthTable expr = [('A', a, b, evaluate expr [('A', a), ('B', b), ('C', c)]) 
                        | a <- [True, False], b <- [True, False], c <- [True, False]]


evaluate :: Expr -> [(Char, Bool)] -> Bool
evaluate (Var x) assignments = case lookup x assignments of
                                    Just value -> value
                                    Nothing -> error ("Variable " ++ show x ++ " not found in assignments")
evaluate (Not expr) assignments = not (evaluate expr assignments)
evaluate (And expr1 expr2) assignments = evaluate expr1 assignments && evaluate expr2 assignments
evaluate (Or expr1 expr2) assignments = evaluate expr1 assignments || evaluate expr2 assignments


evalExpr (Var x) assignments = case lookup x assignments of
                                    Just value -> value
                                    Nothing -> error ("Variable " ++ show x ++ " not found in assignments")
evalExpr (Not expr) assignments = not (evalExpr expr assignments)
evalExpr (And expr1 expr2) assignments = evalExpr expr1 assignments && evalExpr expr2 assignments
evalExpr (Or expr1 expr2) assignments = evalExpr expr1 assignments || evalExpr expr2 assignments
