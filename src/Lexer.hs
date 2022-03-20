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
type LexError = String

data Token
  = Variable String
  | Keyword String
  | InvalidToken Char  -- ^ for lexical error
  | EndOfInput
  deriving Eq

makePrisms ''Token

type TokenWithPos = (Token, Row, Col)

instance Show Token where
  show (Variable v) = "'" ++ v ++ "'"
  show (Keyword kw) = "'" ++ kw ++ "'"
  show (InvalidToken c) = show c
  show EndOfInput = ""

-- | Used by ViewModel to show error messages.
-- When the error is in the term, the error message does not include a row number.
-- When the error is in the definitions, the error message contains both row and column numbers.
showPosition :: TokenWithPos -> Bool -> String
showPosition (tok, row, col) showRow = if showRow then show row ++ ":" ++ show col else show col 

-- | Does not include the character '\', because the updateModel turns all the '\'s into 'λ's.
symbols :: [Char]
symbols = "λ.()=;"

-- | Adds row and column numbers to all characters in the string.
strWithPos :: Row -> Col -> String -> [(Char, Row, Col)]
strWithPos _ _ [] = []
strWithPos row col (c:cs)
  | c == '\n' = (c, row, col) : strWithPos (row+1) 1 cs 
  | otherwise = (c, row, col) : strWithPos row (col+1) cs

isIdentifierChar :: (Char, Row, Col) -> Bool
isIdentifierChar (c, _, _) = (isAlphaNum c && c /= 'λ') || c == '_' || c == '\''

isNewLine :: (Char, Row, Col) -> Bool
isNewLine (c, _, _) = c == '\n'

-- | Converts characters into tokens, the position information is preserved.
charsToTokens :: [(Char, Row, Col)] -> Either (LexError, TokenWithPos) [TokenWithPos]
charsToTokens [] = return []
charsToTokens ((c, row, col) : cs)
  -- whitespaces are ignored
  | isSpace c = charsToTokens cs
  -- identifiers allow letters, digits, underscores and prime symbols, but they can only begin with letters or underscores
  | (isAlpha c && c /= 'λ') || c == '_' = let
    (cs', rest) = span isIdentifierChar cs
    name = c : map (\(c,_,_) -> c) cs'
    in ((Variable name, row, col) :) <$> charsToTokens rest
  -- keywords
  | c `elem` symbols = ((Keyword [c], row, col) :) <$> charsToTokens cs
  -- comments are ignored
  | c == '-'
  , (('-', _, _) : cs') <- cs =
    charsToTokens $ snd $ break isNewLine cs'
  | otherwise = Left ("Found invalid character", (InvalidToken c, row, col))

-- | Converts an input string into tokens.
tokenize :: String -> Either (LexError, TokenWithPos) [TokenWithPos]
tokenize = charsToTokens . strWithPos 1 1