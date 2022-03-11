module Lexer
  ( Token (..),
    tokenize,
  )
where

import Data.Char (isAlpha, isAlphaNum, isSpace)

data Token
  = Variable String
  | Keyword String
  deriving (Eq, Show)

symbols :: [Char]
symbols = "\\λ.()"

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

tokenize :: String -> Either String [Token]
tokenize [] = return []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isAlpha c && c /= 'λ' =
    let (rest, cs') = span isIdentifierChar cs
     in (Variable (c:rest) :) <$> tokenize cs'
  | c `elem` symbols = (Keyword [c] :) <$> tokenize cs
  | otherwise = Left $ show c ++ " is an invalid symbol"