module Main where

import Data.Foldable (toList)
import Control.Monad.Trans.Reader (runReader)

import Lexer
import Parser
import GraphReduction

main :: IO ()
main = case runParser abstraction (tokenize "(\\x.x) y") of
        Left err -> putStrLn err
        Right (ast, _) -> let newAST = runReader (renameFreeVars ast) []
                        in print $ toList $ astToGraph newAST 0

-- some lambda expression
-- ((\\x.\\y.x)((\\w.w)(\\z.z)))(\\u.u)
-- \\f.(\\x.(f (x x))) (\\x.(f (x x)))