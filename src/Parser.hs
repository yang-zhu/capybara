module Parser where

import Lexer
import Control.Applicative (Alternative(..))
import Debug.Trace (trace)

{-
Grammar:
Abstraction ::= "\" Variable "." Abstraction | Application
Application ::= Atom {Atom}  -- left associative
Atom ::= Variable | "(" Abstraction ")"
-}

data Expression = Var String
                | Lambda String Expression
                | App Expression Expression
                deriving Show

type ParseError = String

newtype Parser a = Parser ([Token] -> Either ParseError (a, [Token]))

-- strip off the Parser constructor
runParser :: Parser a -> [Token] -> Either ParseError (a, [Token])
runParser (Parser p) = p

instance Monad Parser where
  return :: a -> Parser a
  return x = Parser $ \ts -> return (x,ts)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \ts0 -> do (actVal, ts1) <- runParser p ts0
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
  empty :: Parser a
  empty = Parser $ \_ -> Left ""
  
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \ts -> case runParser p1 ts of
                                Left _ -> runParser p2 ts
                                Right res -> Right res

-- parse a token
token :: Parser Token
token = Parser $ \ts0 -> case ts0 of
                          [] -> Left "Expected a token, but found end of input."
                          t:ts1 -> Right (t,ts1)

-- check if the to-be-processed token matches the expected keyword
matchKeyword :: String -> Parser ()
matchKeyword tok = do t <- token
                      case t of
                        Keyword t -> if t == tok then return () else empty
                        t -> errorMsg $ "Expected keyword " ++ tok ++ ", but found " ++ show t

-- abort parsing and deliver an error message
errorMsg :: String -> Parser a
errorMsg msg = Parser $ \_ -> Left msg

-- parse an abstraction
abstraction :: Parser Expression
abstraction = do matchKeyword "\\"
                 v <- variable
                 matchKeyword "."
                 e <- abstraction
                 return (Lambda v e)
              <|> application

-- parse an application
application :: Parser Expression
application = some atom >>= return . foldl1 App

-- parse an atomic expression
atom :: Parser Expression
atom = matchKeyword "(" *> abstraction <* matchKeyword ")"
       <|> (variable >>= return . Var)

-- parse a variable
variable :: Parser String
variable = do t <- token
              case t of
                Variable v -> return v
                _ -> errorMsg $ "Expected a variable, but found " ++ show t ++ "."