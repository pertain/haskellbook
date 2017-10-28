-- Hutton's Razor exercises (ch 11)

data Expr = Lit Integer | Add Expr Expr

-- Write the "eval" function which reduces an
-- expression to a final sum
eval :: Expr -> Integer
--eval = undefined
eval (Lit x) = x
eval (Add a b) = eval a + eval b

-- Write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
