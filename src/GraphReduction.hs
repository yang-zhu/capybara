module GraphReduction where

import Control.Monad.Trans.Reader
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (><), (!?))

import Parser


data Node = VarNode String
          | LamNode String Int
          | AppNode Int Int
          deriving Show
type Graph = Seq Node

-- mark free variables with a $-symbol at the beginning to distinguish them from bound variables
renameFreeVars :: Expression -> Reader [String] Expression
renameFreeVars (Lambda v e) = Lambda v <$> local (v:) (renameFreeVars e)
renameFreeVars (Var v) = do
  boundVars <- ask
  if v `elem` boundVars 
    then return $ Var v
    else return $ Var ('$':v)
renameFreeVars (App e1 e2) = App <$> renameFreeVars e1 <*> renameFreeVars e2

-- convert the parse tree to a graph
astToGraph :: Expression -> Int -> Graph
astToGraph (App e1 e2) offset = AppNode (offset+1) (offset+length subgraph1+1) <| subgraph1 >< subgraph2
  where
    subgraph1, subgraph2 :: Graph
    subgraph1 = astToGraph e1 (offset+1)
    subgraph2 = astToGraph e2 (offset+length subgraph1+1)
astToGraph (Lambda v e) offset = LamNode v (offset+1) <| astToGraph e (offset+1)
astToGraph (Var v) _ = Seq.singleton (VarNode v)