module GraphReduction (
  Graph,
  run
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Parser

data Node
  = VarNode String
  | LamNode String Int
  | AppNode Int Int
  deriving (Show, Eq)

type Graph = Seq Node

-- mark free variables with a $-symbol at the beginning to distinguish them from bound variables
renameFreeVars :: Expression -> Reader [String] Expression
renameFreeVars (Var v) = do
  boundVars <- ask
  if v `elem` boundVars
    then return $ Var v
    else return $ Var ('$' : v)
renameFreeVars (Lam v e) = Lam v <$> local (v :) (renameFreeVars e)
renameFreeVars (App e1 e2) = App <$> renameFreeVars e1 <*> renameFreeVars e2

addNode :: Node -> State Graph Int
addNode node = do
  modify (|> node)
  graph <- get
  return (Seq.length graph - 1)

-- convert the parse tree to a graph
astToGraph :: Expression -> State Graph Int
astToGraph (Var v) = addNode (VarNode v)
astToGraph (Lam v e) = do
  addr <- astToGraph e
  addNode (LamNode v addr)
astToGraph (App e1 e2) = do
  addr1 <- astToGraph e1
  addr2 <- astToGraph e2
  addNode (AppNode addr1 addr2)

-- check if the body contains any free occurrences of param
ifParamInBody :: String -> Int -> Reader Graph Bool
ifParamInBody param body = do
  graph <- ask
  case Seq.index graph body of
    VarNode v -> return (v == param)
    LamNode v e -> do
      res <- ifParamInBody param e
      return $ (v /= param) && res
    AppNode e1 e2 -> do
      res1 <- ifParamInBody param e1
      res2 <- ifParamInBody param e2
      return $ res1 || res2

instantiate :: String -> Int -> Int -> State Graph Int
instantiate param body arg = do
  graph <- get
  let paramInBody = runReader (ifParamInBody param body) graph
  if paramInBody
    then case Seq.index graph body of
      VarNode v
        | v == param -> return arg
        | otherwise -> return body
      LamNode v e
        | v == param -> return body
        | otherwise -> do
          e' <- instantiate param e arg
          addNode (LamNode v e')
      AppNode e1 e2 -> do
        e1' <- instantiate param e1 arg
        e2' <- instantiate param e2 arg
        addNode (AppNode e1' e2')
    else return body

-- search for redex in an expression and do one reduction
redex :: Int -> State Graph Bool
redex root = do
  graph <- get
  case Seq.index graph root of
    AppNode e1 e2 -> case Seq.index graph e1 of
      LamNode param body -> do
        inst <- instantiate param body e2
        graph' <- get
        modify (Seq.update root (Seq.index graph' inst))
        return True
      _ -> redex e1
    _ -> return False

-- reduce the complete graph
reduce :: Int -> Graph -> [Graph]
reduce root graph =
  let (reduced, graph') = runState (redex root) graph
   in if reduced
        then graph : reduce root graph'
        else [graph]

-- run the reduction
run :: Expression -> [Graph]
run expr =
  let renamedExpr = runReader (renameFreeVars expr) []
      (root, graph) = runState (astToGraph renamedExpr) Seq.empty
   in reduce root graph