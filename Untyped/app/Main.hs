module Main where

import Data.Text (pack)
import Syntax
import Parser
import Eval

-- Top level
main :: IO ()
main = do
    let input = pack "(\\x . x (x y)) N"
    putStrLn "Result from parsing:"
    parseLineIO input
    putStrLn "Result from applying beta reductions:"
    case parseLine input of
        Left err -> putStrLn "No reductions to perform."
        Right expr -> print $ eval expr