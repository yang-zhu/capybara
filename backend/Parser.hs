module Parser
  ( Definition(..)
  , Expression (..)
  , ParseError (..)
  , parseDefinitions
  , parseExpression
  , abstraction
  , _ExprError
  , _DefError
  )
where

import Control.Applicative (Alternative (..))
import Control.Lens (makePrisms)

import Lexer


{-|
  Grammar:
  Program ::= Definition {Definition}
  Definition ::= Variable "=" Abstraction ";"
  Abstraction ::= "λ" Variable "." Abstraction | Application
  Application ::= Atom {Atom}  -- left associative
  Atom ::= Variable | "(" Abstraction ")"
-}

data Definition = Def String Expression
  deriving Eq

data Expression
  = Var String
  | Lam String Expression
  | App Expression Expression
  deriving (Show, Eq)

data ParseError
  = ExprError (String, TokenWithPos)
  | DefError (String, TokenWithPos)
  deriving Eq

makePrisms ''ParseError

newtype Parser a = Parser ([TokenWithPos] -> Either (String, TokenWithPos) (a, [TokenWithPos]))

instance Monad Parser where
  return :: a -> Parser a
  return x = Parser $ \ts -> return (x, ts)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \ts0 -> do
    (actVal, ts1) <- runParser p ts0
    runParser (f actVal) ts1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = p >>= return . f

instance Applicative Parser where
  pure :: a -> Parser a
  pure = return

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  mf <*> mp = mf >>= \f -> mp >>= return . f

instance Alternative Parser where
  -- | This function should not be used. Left ("unknown", (EndOfInput,0,0)) is a dummy value to fill the gap.
  empty :: Parser a
  empty = Parser $ \_ -> Left ("unknown", (EndOfInput,0,0))

  -- | If p1 already managed to parse some tokens, then p2 will not be tried.
  -- This is to prevent full backtracking.
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \ts -> case runParser p1 ts of
    err@(Left (_, tok)) -> if null ts || tok == head ts then runParser p2 ts else err
    Right res -> Right res


parse :: Parser a -> String -> Either (String, TokenWithPos) a
parse p input = case tokenize input >>= runParser p of
  Left err -> Left err
  Right (res, rest) -> if null rest then Right res else Left ("Expected end of input", head rest)

parseDefinitions :: String -> Either ParseError [Definition]
parseDefinitions defs = either (Left . DefError) Right (parse (some definition) defs)

parseExpression :: String -> Either ParseError Expression
parseExpression expr = either (Left . ExprError) Right (parse abstraction expr)

-- | Strips off the Parser constructor.
runParser :: Parser a -> [TokenWithPos] -> Either (String, TokenWithPos) (a, [TokenWithPos])
runParser (Parser p) = p

-- | Parses a token.
token :: Parser TokenWithPos
token = Parser $ \case
  -- the two 0s are dummy positions
  -- the reason that in this case it does not return a Left value immediately is to allow matchKeyword to generate a better error message
  [] -> Right ((EndOfInput,0,0), [])
  t : ts1 -> Right (t, ts1)

-- | Checks if the to-be-processed token matches the expected keyword.
matchKeyword :: String -> Parser ()
matchKeyword tok = token >>= \case
  (Keyword kw, _, _)
    | kw == tok -> return ()
  t -> errorMsg  ("Expected " ++ show tok) t

-- | Aborts parsing and delivers an error message together with the erroneous token.
-- The erroneous token is used to generate a more concrete error message. See composeErrorMsg in ViewModel.
errorMsg :: String -> TokenWithPos ->  Parser a
errorMsg msg tok = Parser $ \_ -> Left (msg, tok)

-- | Parses a definition.
definition :: Parser Definition
definition = do
  fun <- variable
  matchKeyword "="
  body <- abstraction
  matchKeyword ";"
  return (Def fun body)

-- | Parses an abstraction
abstraction :: Parser Expression
abstraction = do
    matchKeyword "λ"
    v <- variable
    matchKeyword "."
    e <- abstraction
    return (Lam v e)
    <|> application

-- Parses an application
application :: Parser Expression
application = some atom >>= return . foldl1 App

-- Parses an atomic expression
atom :: Parser Expression
atom = matchKeyword "(" *> abstraction <* matchKeyword ")"
  <|> (variable >>= return . Var)

-- Parses a variable
variable :: Parser String
variable = token >>= \case
  (Variable v, _, _) -> return v
  t -> errorMsg "Expected a variable" t