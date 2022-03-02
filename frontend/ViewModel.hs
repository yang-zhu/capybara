module ViewModel(viewModel) where

import Data.Foldable(toList)
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Miso
import Miso.String
import Miso.Svg

import GraphReduction
import Model


type Depth = Int
type XCoord = Double

header :: View Action
header = h1_ [] [text "Capy-Lambda" ]

formButtons :: View Action
formButtons = div_ [class_ "btn-group"] [
  button_ [type_ "button", class_ "btn btn-success", onClick Eval] [text "evaluate"],
  button_ [type_ "button", class_ "btn btn-secondary", onClick Clear] [text "clear"]
  ]

form :: Model -> View Action
form Model{input} = div_ [class_ "form"] [
  input_ [type_ "text", class_ "form-control", placeholder_ "(\\x.x) y", value_ input, onInput TextInput],
  formButtons
  ]

-- renderGraph :: Either String [Graph] -> View Action
-- renderGraph (Right graphs) = ul_ [] [li_ [] [(text . ms . show .toList) graph] | graph <- graphs]
-- renderGraph (Left err) = text (ms err)

computeDepths :: (Int, Graph) -> Seq Depth -> Depth -> Seq Depth
computeDepths (root, graph) depths currDepth
  | currDepth > Seq.index depths root = case Seq.index graph root of
      VarNode v -> depths'
      LamNode v e -> computeDepths (e, graph) depths' currDepth'
      AppNode e1 e2 -> let
        depths'' = computeDepths (e1, graph) depths' currDepth'
        in computeDepths (e2, graph) depths'' currDepth'
  | otherwise = depths
  where
    depths' = Seq.update root currDepth depths 
    currDepth' = currDepth + 1

layout :: (Int, Graph) -> Seq XCoord -> XCoord -> (XCoord, XCoord, Seq XCoord)
layout (root, graph) xcoords leftEdge
  | Seq.index xcoords root == -1 = case Seq.index graph root of
      VarNode v -> (leftEdge+4, leftEdge+2, Seq.update root (leftEdge+2) xcoords)
      LamNode v e -> let
        (eRightEdge, eRoot, xcoords') = layout (e, graph) xcoords leftEdge
        in (eRightEdge, eRoot, Seq.update root eRoot xcoords')
      AppNode e1 e2 -> let
        (e1RightEdge, e1Root, xcoords') = layout (e1, graph) xcoords leftEdge
        (e2RightEdge, e2Root, xcoords'') = layout (e2, graph) xcoords' e1RightEdge
        rootXCoord = (e1Root + e2Root) / 2
        in (e2RightEdge, rootXCoord, Seq.update root rootXCoord xcoords'')
  | otherwise = (leftEdge, leftEdge, xcoords)

draw :: (Int, Graph) -> Seq Depth -> Seq XCoord -> [View Action]
draw (root, graph) depths xcoords = case Seq.index graph root of
  VarNode v -> [text_ [x_ (ms rootX), y_ (ms rootY),fill_ "black"] [text (ms v)]]
  LamNode v e -> let
    eX = 50 + Seq.index xcoords e
    eY = 50 * Seq.index depths e
    in [ text_ [x_ (ms rootX), y_ (ms rootY)] [text ("\\"<> ms v)]
       , line_ [x1_ (ms rootX), x2_ (ms eX), y1_ (ms rootY), y2_ (ms eY), stroke_ "black", strokeWidth_ "1"] []
       ] ++ draw (e, graph) depths xcoords
  AppNode e1 e2 -> let
    e1X = 50 + Seq.index xcoords e1
    e1Y = 50 * Seq.index depths root
    e2X = 50 + Seq.index xcoords e2
    e2Y = 50 * Seq.index depths root
    in [ text_ [x_ (ms rootX), y_ (ms rootY)] [text "@"]
        , path_ [d_ ("M " <> ms rootX <> " " <> ms rootY <> "Q " <> ms (rootX-10) <> ms (rootY+10) <> ms e1X <> ms e1Y), stroke_ "black", strokeWidth_ "1"] []
        , path_ [d_ ("M " <> ms rootX <> " " <> ms rootY <> "Q " <> ms (rootX+10) <> ms (rootY+10) <> ms e2X <> ms e2Y), stroke_ "black", strokeWidth_ "1"] []
        ] ++ draw (e1, graph) depths xcoords ++ draw (e2, graph) depths xcoords
  where
    rootX = 50 + Seq.index xcoords root
    rootY = 50 * Seq.index depths root

renderGraph :: Either String (Int, [Graph]) -> Int -> View Action
renderGraph (Right (root, graphs)) index = let
  graph = graphs !! index
  depths = computeDepths (root, graph) (Seq.replicate (Seq.length graph) (-1)) 0
  (_, _, xcoords) = layout (root, graph) (Seq.replicate (Seq.length graph) (-1)) 0
  in svg_ [Miso.Svg.width_ "1000", Miso.Svg.height_ "1000", viewBox_ "0 0 950 950"] (draw (root, graph) depths xcoords) 
renderGraph (Left err) _ = text (ms err)

gridSlider :: View Action
gridSlider = div_ [] [input_ [type_ "range", orient_ "vertical"]]

gridButtons :: View Action
gridButtons = div_ [class_ "btn-group-vertical"] [
  button_ [type_ "button", class_ "btn btn-outline-primary", onClick Prev] [text "prev"],
  button_ [type_ "button", class_ "btn btn-outline-primary", onClick Next] [text "next"]
  ]

gridGraph :: Model -> View Action
gridGraph Model{output, graphIndex} = div_ [class_ "grid-container"]
  [ gridSlider
  , renderGraph output graphIndex
  , gridButtons
  ]

viewModel :: Model -> View Action
viewModel m@(Model input output index) = div_ []
  [ header
  , form m
  , gridGraph m
  ]