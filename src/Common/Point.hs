module Common.Point (
    Token(..),
    Terminal(..),
    NonTerm,
    Point(..),
) where


data Token = Token
    deriving (Eq, Ord)

data Terminal
    = TokenTerm Token
    | NullTerm
    deriving (Eq, Ord)

type NonTerm = String -- Name of the referenced rule

data Point
    = Terminal Terminal
    | NonTerm NonTerm
    deriving (Eq, Ord)
