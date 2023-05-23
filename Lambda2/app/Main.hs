module Main where

import Data.Text (pack)
import Syntax
import Types
import Parser
import TypeSystem

-- Top level
main :: IO ()
main = do
    let input = pack "(\\x:^a:* -> b. x)"
    putStrLn "Result from parsing:"
    parseLineIO input
    putStrLn "Result from type system:"
    let ast = parseLine input
    case ast of
        Left _ -> putStrLn "No types to check."
        Right expr -> case typecheck [] expr of 
            Left err -> print err
            Right (env, t) -> do
                putStrLn $ "Type of expression: " ++ show t
                putStrLn $ "Type Environment = " ++ show' env