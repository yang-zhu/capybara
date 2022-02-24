module Lexer
    ( Token(..)
    , tokenize
    ) where

import Data.Char (isSpace, isAlpha, isAlphaNum)


data Token 
    = Variable String
    | Keyword String
    deriving (Eq, Show)

symbols = "\\.()"

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isAlpha c =
        let (rest, cs') = span isIdentifierChar cs
        in Variable (c:rest) : tokenize cs'
    | c `elem` symbols = Keyword [c] : tokenize cs
    | otherwise = error "invalid input"