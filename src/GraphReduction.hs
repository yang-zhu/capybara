module GraphReduction where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><), (!?))

import Parser


data Node = VarNode String
          | LamNode String Int
          | AppNode Int Int
          deriving Show
type Graph = Seq Node

-- mark free variables with a $-symbol at the beginning to distinguish them from bound variables
renameFreeVars :: Expression -> Reader [String] Expression
renameFreeVars (Var v) = do
  boundVars <- ask
  if v `elem` boundVars 
    then return $ Var v
    else return $ Var ('$':v)
renameFreeVars (Lam v e) = Lam v <$> local (v:) (renameFreeVars e)
renameFreeVars (App e1 e2) = App <$> renameFreeVars e1 <*> renameFreeVars e2

addNode :: Node -> State Graph Int
addNode node = do modify (|> node)
                  graph <- get
                  return (Seq.length graph - 1)

-- convert the parse tree to a graph
astToGraph :: Expression -> State Graph Int
astToGraph (Var v) = addNode (VarNode v)
astToGraph (Lam v e) = do addr <- astToGraph e
                          addNode (LamNode v addr)
astToGraph (App e1 e2) = do addr1 <- astToGraph e1
                            addr2 <- astToGraph e2
                            addNode (AppNode addr1 addr2)

instantiate :: Int -> String -> Int -> State Graph Int
instantiate body param arg = do
  graph <- get
  case Seq.index graph body of
    VarNode v -> if v == param then return arg else return body
    LamNode v e -> if v == param then return body else 
      do e' <- instantiate e param arg
         addNode (LamNode v e')
    AppNode e1 e2 -> do e1' <- instantiate e1 param arg
                        e2' <- instantiate e2 param arg
                        addNode (AppNode e1' e2')

-- search for redex in an expression and do one reduction
redex :: Int -> State Graph Bool
redex root = do
  graph <- get
  case Seq.index graph root of
    AppNode e1 e2 -> case Seq.index graph e1 of
      LamNode param body -> do inst <- instantiate body param e2
                               graph' <- get
                               modify (Seq.update root (Seq.index graph' inst))
                               return True
      others -> redex e1
    others -> return False