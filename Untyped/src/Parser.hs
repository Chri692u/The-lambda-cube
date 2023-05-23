{-# LANGUAGE OverloadedStrings #-}

module Parser(parseLine, parseLineIO) where

import Data.Text (Text)
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Syntax
import Lexer

-- Variable parser
variable :: Parser Expr
variable = do
    v <- lexeme $ some letterChar
    return $ Var v

-- Abstraction parser
abstraction :: Parser Expr
abstraction = do
    symbol "\\"
    var <- lexeme (some letterChar)
    symbol "."
    Abs var <$> expr

-- Term parser
term :: Parser Expr
term = choice [
        parens expr,
        variable,
        abstraction
    ]

-- Parse expressions
expr :: Parser Expr
expr = do
    es <- many term
    return (foldl1 App es)

-- Top level
parseLine :: Text -> Either (ParseErrorBundle Text Void) Expr
parseLine = parse (spaces *> expr <* eof) ""

parseLineIO :: Text -> IO ()
parseLineIO input = case parseLine input of
        Left err -> putStrLn $ "Parser error on line " ++ errorBundlePretty err
        Right e  -> print e