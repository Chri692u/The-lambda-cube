{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Text (Text)
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char(space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Lexer for consuming whitespace and comments
spaces :: Parser ()
spaces = L.space space1 lComment bComment
    where lComment = L.skipLineComment "--"
          bComment = L.skipBlockComment "{-" "-}"

-- Tokens
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

star :: Parser Text
star = symbol "*"

otherkind :: Parser Text
otherkind = symbol "?"