module GraphReduction
  ( Node(..)
  , Graph
  , EvalStrategy(..)
  , nodes
  , counter
  , run
  ) where

import Data.Maybe (isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Lens (makeLenses, (^.), (.~), (%~))

import Parser


data Node
  = VarNode String
  | LamNode String Int
  | AppNode Int Int
  deriving (Show, Eq)

data Graph = Graph
  { _nodes :: Seq Node
  , _counter :: Int
  }
  deriving Eq

makeLenses ''Graph

data EvalStrategy
  = CallByName
  | CallByValue
  | CallByNeed
  deriving (Show, Eq)


digitToSubscript :: Char -> Char
digitToSubscript '0' = '₀'
digitToSubscript '1' = '₁'
digitToSubscript '2' = '₂'
digitToSubscript '3' = '₃'
digitToSubscript '4' = '₄'
digitToSubscript '5' = '₅'
digitToSubscript '6' = '₆'
digitToSubscript '7' = '₇'
digitToSubscript '8' = '₈'
digitToSubscript '9' = '₉'
digitToSubscript c = c

alphaRename :: Expression -> ReaderT [(String, String)] (State Int) Expression
alphaRename (Var v) = do
  renamings <- ask
  case lookup v renamings of
    Nothing -> return (Var v)
    Just v' -> return (Var v')
alphaRename (Lam v e) = do
  counter <- lift get
  lift (modify (+1))
  let v' = v ++ map digitToSubscript (show counter)
  e' <- local ((v, v'):) (alphaRename e)
  return (Lam v' e')
alphaRename (App e1 e2) = App <$> alphaRename e1 <*> alphaRename e2

addNode :: Node -> State Graph Int
addNode node = do
  modify $ nodes %~ (|> node)
  graph <- get
  return (Seq.length (graph^.nodes) - 1)

getNode :: Int -> State Graph Node
getNode index = do
  graph <- get
  return (Seq.index (graph^.nodes) index)

updateNode :: Int -> Int -> State Graph ()
updateNode old new = do
  graph <- get
  modify $ nodes %~ Seq.update old (Seq.index (graph^.nodes) new)

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
  case Seq.index (graph^.nodes) body of
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
oneStepReduce :: Int -> [Definition] -> ReaderT EvalStrategy (State Graph) (Bool, Maybe Int)
oneStepReduce root defs = do
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
          return (True, Just root)
        else oneStepReduce e2 defs
      _ -> oneStepReduce e1 defs
    VarNode v -> case lookUpInDefs v defs of
      Just bodyExpr -> do
        graph <- lift get
        let (bodyExpr', counter') = runState (runReaderT (alphaRename bodyExpr) []) (graph^.counter)
        lift $ modify $ counter .~ counter'
        body <- lift $ astToGraph bodyExpr'
        lift $ updateNode root body
        return (True, Just root)
      Nothing -> return (False, Nothing)
    _ -> return (False, Nothing)
  where
    isValue :: Node -> Bool
    isValue (AppNode _ _) = False
    isValue (VarNode v) = not $ isDef v defs
    isValue _ = True

    lookUpInDefs :: String -> [Definition] -> Maybe Expression
    lookUpInDefs _ [] = Nothing
    lookUpInDefs var ((Def fun body):defs) = if var == fun then Just body else lookUpInDefs var defs

    isDef :: String -> [Definition] -> Bool
    isDef = (isJust .) . lookUpInDefs

-- reduce the complete graph
reduce :: EvalStrategy -> Int -> Graph -> [Definition] -> [(Graph, Maybe Int)]
reduce strat root graph defs = let
   ((reduced, redex), graph') = runState (runReaderT (oneStepReduce root defs) strat) graph
   in if reduced
        then (graph, redex) : reduce strat root graph' defs
        else [(graph, redex)]

-- run the reduction
run :: EvalStrategy -> Expression -> [Definition] -> (Int, [(Graph, Maybe Int)])
run strat expr defs = let
  (renamedExpr, counter) = runState (runReaderT (alphaRename expr) []) 1
  (root, graph) = runState (astToGraph renamedExpr) Graph{_nodes=Seq.empty, _counter=counter}
  in (root, reduce strat root graph defs)