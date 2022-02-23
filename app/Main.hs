module Main where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Reader (runReader)
import Control.Monad.Trans.State (runState)

import Lexer
import Parser
import GraphReduction

main :: IO ()
main = case runParser abstraction (tokenize "((\\x.\\y.x)((\\w.w)(\\z.z)))(\\u.u)") of
        Left err -> putStrLn err
        Right (ast, _) -> let graphs = map toList (run ast)
                           in mapM_ print graphs

-- some lambda expression
-- (\\x.(x x))(\\y.y)
-- ((\\x.\\y.x)((\\w.w)(\\z.z)))(\\u.u)
-- \\f.(\\x.(f (x x))) (\\x.(f (x x)))