module Main (main) where

import System.Exit (exitSuccess)

import Parser


main :: IO ()
main = do
    ln <- getLine
    if ln == "quit" || ln == "q" then
        exitSuccess
    else do
        let tree = parseCalc ln
        let res = eval tree
        print res
        main
