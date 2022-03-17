module Lexer
  ( Token(..)
  , TokenWithPos
  , _InvalidToken
  , showPosition
  , tokenize
  )
where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Control.Lens (makePrisms)

type Row = Int
type Col = Int
type Pos = (Row, Col)
type LexError = String

data Token
  = Variable String
  | Keyword String
  | InvalidToken Char
  | EndOfInput
  deriving Eq

makePrisms ''Token

type TokenWithPos = (Token, Row, Col)

instance Show Token where
  show (Variable v) = "'" ++ v ++ "'"
  show (Keyword kw) = "'" ++ kw ++ "'"
  show (InvalidToken c) = show c
  show EndOfInput = ""


showPosition :: TokenWithPos -> Bool -> String
showPosition (tok, row, col) showRow = if showRow then show row ++ ":" ++ show col else show col 

symbols :: [Char]
symbols = "λ.()=;"

strWithPos :: Row -> Col -> String -> [(Char, Row, Col)]
strWithPos _ _ [] = []
strWithPos row col (c:cs)
  | c == '\n' = (c, row, col) : strWithPos (row+1) 1 cs 
  | otherwise = (c, row, col) : strWithPos row (col+1) cs

isIdentifierChar :: (Char, Row, Col) -> Bool
isIdentifierChar (c, _, _) = isAlphaNum c || c == '_'

charsToTokens :: [(Char, Row, Col)] -> Either (LexError, TokenWithPos) [TokenWithPos]
charsToTokens [] = return []
charsToTokens ((c, row, col) : cs)
  | isSpace c = charsToTokens cs
  | isAlpha c && c /= 'λ' = let
    (cs', rest) = span isIdentifierChar cs
    name = c : map (\(c,_,_) -> c) cs'
    in ((Variable name, row, col) :) <$> charsToTokens rest
  | c `elem` symbols = ((Keyword [c], row, col) :) <$> charsToTokens cs
  | otherwise = Left ("Found invalid character", (InvalidToken c, row, col))

tokenize :: String -> Either (LexError, TokenWithPos) [TokenWithPos]
tokenize = charsToTokens . strWithPos 1 1