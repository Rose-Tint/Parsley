module Lexer (tokenize) where

import Data.Char

import AbsSyn


tokenize :: String -> [Token]
tokenize

nextToken :: String -> (String, Token)
nextToken [] = ([], T_EOF)
nextToken (c:cs)
    | isSpace c = nextToken cs
    | isDigit c = 
        let (digs, rest) = span isDigit cs
            num = foldr (\ch acc -> acc `seq`
                acc * 10 + digitToInt ch
                ) (0 :: Int) (c:digs)
        in (rest, T_Int (fromIntegral num))
    | c == '+' = (cs, T_Plus)
    | c == '-' = (cs, T_Minus)
    | otherwise = error $!
        "lexing error: unrecognized character ("
        ++ show c ++ ")"
