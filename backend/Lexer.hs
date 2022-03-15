module Lexer
  ( Token (..)
  , tokenize
  )
where

import Data.Char (isAlpha, isAlphaNum, isSpace)


type LexError = String

data Token
  = Variable String
  | Keyword String
  deriving Eq

instance Show Token where
  show (Variable v) = "variable " ++ v
  show (Keyword kw) = "'" ++ kw ++ "'"


symbols :: [Char]
symbols = "λ.()=;"

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

tokenize :: String -> Either LexError [Token]
tokenize [] = return []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isAlpha c && c /= 'λ' =
    let (rest, cs') = span isIdentifierChar cs
     in (Variable (c:rest) :) <$> tokenize cs'
  | c `elem` symbols = (Keyword [c] :) <$> tokenize cs
  | otherwise = Left $ show c ++ " is an invalid symbol"