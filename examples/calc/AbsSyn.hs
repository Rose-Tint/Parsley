module Examples.Calc.Token (
    Token(..),
    Expr(..),
) where


data Token
    = T_Int Int
    | T_Plus
    | T_Minus
    | T_EOF

data Expr
    = Int Int
    | Add Expr Expr
    | Sub Expr Expr
