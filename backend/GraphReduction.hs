module GraphReduction (
  Node(..),
  Graph,
  EvalStrategy(..),
  run
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Parser
import Control.Monad.Trans.Class (lift)

data Node
  = VarNode String
  | LamNode String Int
  | AppNode Int Int
  deriving (Show, Eq)

type Graph = Seq Node

data EvalStrategy
  = CallByName
  | CallByValue
  | CallByNeed
  deriving (Show, Eq)

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

getNode :: Int -> State Graph Node
getNode index = do
  graph <- get
  return (Seq.index graph index)

updateNode :: Int -> Int -> State Graph ()
updateNode old new = do
  graph <- get
  modify (Seq.update old (Seq.index graph new))

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

copy :: Int -> State Graph Int
copy index = do
  node <- getNode index
  copyNode node

copyNode :: Node -> State Graph Int
copyNode node@(VarNode v) = addNode node
copyNode (LamNode v e) = do
  e' <- copy e
  addNode (LamNode v e')
copyNode (AppNode e1 e2) = do
  e1' <- copy e1
  e2' <- copy e2
  addNode (AppNode e1' e2')

instantiate :: String -> Int -> Int -> ReaderT EvalStrategy (State Graph) Int
instantiate param body arg = do
  strat <- ask
  graph <- lift get
  let paramInBody = runReader (ifParamInBody param body) graph
  if paramInBody
    then lift (getNode body) >>= \case
      VarNode v
        | v == param -> if strat == CallByNeed then return arg else lift (copy arg)
        | otherwise -> return body
      LamNode v e
        | v == param -> return body
        | otherwise -> do
            e' <- instantiate param e arg
            lift $ addNode (LamNode v e')
      AppNode e1 e2 -> do
        e1' <- instantiate param e1 arg
        e2' <- instantiate param e2 arg
        lift $ addNode (AppNode e1' e2')
    else return body

-- search for redex in an expression and do one reduction
redex :: Int -> ReaderT EvalStrategy (State Graph) Bool
redex root = do
  strat <- ask
  rootNode <- lift (getNode root)
  case rootNode of
    AppNode e1 e2 -> lift (getNode e1) >>= \case
      LamNode param body -> do
        e2IsValue <- isValue <$> lift (getNode e2)
        if strat == CallByName || strat == CallByNeed || (strat == CallByValue && e2IsValue)
        then do
          inst <- instantiate param body e2
          lift $ updateNode root inst
          return True
        else redex e2
      _ -> redex e1
    _ -> return False
  where
    isValue :: Node -> Bool
    isValue (AppNode _ _) = False
    isValue _ = True

-- reduce the complete graph
reduce :: EvalStrategy -> Int -> Graph -> [Graph]
reduce strat root graph =
  let (reduced, graph') = runState (runReaderT (redex root) strat) graph
   in if reduced
        then graph : reduce strat root graph'
        else [graph]

-- run the reduction
run :: EvalStrategy -> Expression -> (Int, [Graph])
run strat expr =
  let renamedExpr = runReader (renameFreeVars expr) []
      (root, graph) = runState (astToGraph renamedExpr) Seq.empty
   in (root, reduce strat root graph)