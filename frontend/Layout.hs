module Layout
  ( Depth
  , XCoord
  , computeDepths
  , layout
  ) where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import GraphReduction


type Depth = Int
type XCoord = Double


-- | Computes depths of all the nodes in a graph.
computeDepths :: Int -> Depth -> ReaderT Graph (State (Seq Depth)) ()
computeDepths root currDepth = do
  graph <- ask
  depths <- lift get
  when (currDepth > Seq.index depths root)
    do
      lift $ modify $ Seq.update root currDepth
      case Seq.index (graph^.nodes) root of
        VarNode v -> return ()
        LamNode v e -> computeDepths e currDepth'
        AppNode e1 e2 -> computeDepths e1 currDepth' >> computeDepths e2 currDepth'
  where
    currDepth' = currDepth + 1

-- generated using https://bl.ocks.org/tophtucker/62f93a4658387bb61e4510c37e2e97cf
letterWidthTable = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.2490234375,0.33359375,0.40859375,0.5,0.5,0.83359375,0.778125,0.18046875,0.33359375,0.33359375,0.5,0.5640625,0.25,0.33359375,0.25,0.278125,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.278125,0.278125,0.5640625,0.5640625,0.5640625,0.44453125,0.92109375,0.72265625,0.6671875,0.6671875,0.72265625,0.6109375,0.55625,0.72265625,0.72265625,0.33359375,0.38984375,0.72265625,0.6109375,0.88984375,0.72265625,0.72265625,0.55625,0.72265625,0.6671875,0.55625,0.6109375,0.72265625,0.72265625,0.94453125,0.72265625,0.72265625,0.6109375,0.33359375,0.33984375,0.33359375,0.46953125,0.5,0.343359375,0.44453125,0.5,0.44453125,0.5,0.44453125,0.3828125,0.5,0.5,0.278125,0.3357421875,0.5,0.278125,0.778125,0.5,0.5,0.5,0.5,0.33642578125,0.38984375,0.27880859375,0.5,0.5,0.72265625,0.5,0.5,0.44453125,0.48046875,0.20078125,0.48046875,0.54140625]
defaultLetterWidth = 0.5122738486842104

-- | Safe version of (!!) operator
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:xs) !? i
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

-- | Computes the width of an identifier
stringWidth :: String -> Double
stringWidth s = 2 * foldl (\acc c -> charWidth c + acc) 0 s
  where
    charWidth :: Char -> Double
    charWidth c = fromMaybe defaultLetterWidth (letterWidthTable !? fromEnum c)

-- | Computes x-coordinates of all the nodes in a graph.
layout :: Int       -- ^ the root index of the current subgraph
       -> Depth     -- ^ the current depth
       -> [XCoord]  -- ^ infinite list of left boundaries for new nodes, the i-th element refers to currDepth+i
       -> ReaderT Graph (ReaderT (Seq Depth) (State (Seq XCoord))) ([XCoord], XCoord)
                    -- ^ the result is (the right boundary of the processed nodes, the position of the root node of the subgraph)                  
layout root currDepth (leftEdge0:leftEdge1:leftEdges) = do
  graph <- ask
  depths <- lift ask
  xcoords <- lift2 get
  if Seq.index xcoords root == -1 && Seq.index depths root == currDepth
    then case Seq.index (graph^.nodes) root of
      VarNode v -> do
        let nodeWidth = max 4 (stringWidth v)
        lift2 $ modify $ Seq.update root (leftEdge0+nodeWidth/2)
        return ((leftEdge0+nodeWidth):leftEdge1:leftEdges, leftEdge0+nodeWidth/2)  -- (right edges, root)
      LamNode v e -> do
        (eRightEdges, eRoot) <- layout e currDepth' (max leftEdge0 leftEdge1 : leftEdges)
        lift2 $ modify $ Seq.update root eRoot
        return ((eRoot+2):eRightEdges, eRoot)
      AppNode e1 e2 ->
        if e1 == e2
          then do
            (eRightEdges, eRoot) <- layout e1 currDepth' (max leftEdge0 leftEdge1 : leftEdges)
            lift2 $ modify $ Seq.update root eRoot
            return ((eRoot+2):eRightEdges, eRoot)
          else do
            (e1RightEdges, e1Root) <- layout e1 currDepth' (max (leftEdge0-2) leftEdge1:leftEdges)
            (e2RightEdges, e2Root) <- layout e2 currDepth' e1RightEdges
            let rootXCoord = (e1Root + e2Root) / 2
            lift2 $ modify $ Seq.update root rootXCoord
            return ((rootXCoord+2):e2RightEdges, rootXCoord)
    else return ((leftEdge0+4):leftEdge1:leftEdges, leftEdge0+2)
  where
    lift2 = lift . lift
    currDepth' = currDepth + 1
layout _ _ _ = undefined