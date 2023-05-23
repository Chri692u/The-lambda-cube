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
import qualified Data.Functor
import Data.Functor (($>))

-- type literal
lit :: Parser Expr
lit = do
    var <- lexeme $ some letterChar
    symbol ":"
    t <- types
    return $ Lit (var,t)

-- Variable parser
variable :: Parser Expr
variable = do
    var <- lexeme $ some letterChar
    return $ Var var

-- Abstraction parser
abstraction :: Parser Expr
abstraction = do
    symbol "\\"
    var <- lexeme $ some letterChar
    symbol ":"
    t <- types
    symbol "."
    Abs var t <$> expr

-- Term parser
term :: Parser Expr
term = choice [
        parens expr,
        try lit,
        variable,
        abstraction
    ]

-- Parse expressions
expr :: Parser Expr
expr = do
    es <- many term
    return (foldl1 App es)

-- Type parsers
tAtom :: Parser Type
tAtom = choice [symbol "bool" $> TBool,
                symbol "int" $> TInt,
                parens types]

types :: Parser Type
types = makeExprParser tAtom tyops
  where
    infixOp x f = InfixR (symbol x $> f)
    tyops = [[infixOp "->" TArr]]

-- Top level
parseLine :: Text -> Either (ParseErrorBundle Text Void) Expr
parseLine = parse (spaces *> expr <* eof) ""

parseLineIO :: Text -> IO ()
parseLineIO input = case parseLine input of
        Left err -> putStrLn $ "Parser error on line " ++ errorBundlePretty err
        Right e  -> print e