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

buttons :: View Action
buttons = div_ [class_ "btn-group"] [
  button_ [type_ "button", class_ "btn btn-success", onClick Eval] [text "evaluate"],
  button_ [type_ "button", class_ "btn btn-secondary", onClick Clear] [text "clear"]
  ]

form :: Model -> View Action
form (Model input _) = div_ [class_ "form"] [
  input_ [type_ "text", class_ "form-control", placeholder_ "(\\x.x) y", value_ input, onInput TextInput],
  buttons
  ]

renderGraph :: Either String [Graph] -> View Action
renderGraph (Right graphs) = ul_ [] [li_ [] [(text . ms . show .toList) graph] | graph <- graphs]
renderGraph (Left err) = text (ms err)

viewModel :: Model -> View Action
viewModel m@(Model input output) = div_ [] [
  header,
  form m,
  div_ [] [renderGraph output]
  ]

-- layout :: Graph -> Int -> Int -> Int -> [View Action]
-- layout graph root x y = case Seq.index graph root of
--   VarNode v -> [text_ [x_ (ms x), y_ (ms (y+10)),fill_ "black"] [text (ms v)]]
--   LamNode v e ->
--     [ text_ [x_ (ms x), y_ (ms y)] [text ("\\"<> ms v)]
--     , line_ [x1_ (ms x), x2_ (ms x), y1_ (ms y), y2_ (ms (y+10)), stroke_ "black", strokeWidth_ "3"] []
--     ] ++ layout graph e x (y+10)
--   AppNode e1 e2 ->
--     [ text_ [x_ (ms x), y_ (ms y)] [text "@"]
--     , line_ [x1_ (ms x), x2_ (ms (x-7)), y1_ (ms y), y2_ (ms (y+10)), stroke_ "black", strokeWidth_ "3"] []
--     , line_ [x1_ (ms x), x2_ (ms (x+7)), y1_ (ms y), y2_ (ms (y+10)), stroke_ "black", strokeWidth_ "3"] []
--     ] ++ layout graph e1 (x-7) (y+10) ++ layout graph e2 (x+7) (y+10)

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
      VarNode v -> (leftEdge+1, leftEdge+0.5, Seq.update root (leftEdge+0.5) xcoords)
      LamNode v e -> let
        (eRightEdge, eRoot, xcoords') = layout (e, graph) xcoords leftEdge
        in (eRightEdge, eRoot, Seq.update root eRoot xcoords')
      AppNode e1 e2 -> let
        (e1RightEdge, e1Root, xcoords') = layout (e1, graph) xcoords leftEdge
        (e2RightEdge, e2Root, xcoords'') = layout (e2, graph) xcoords' e1RightEdge
        rootXCoord = (e1Root + e2Root) / 2
        in (e2RightEdge, rootXCoord, Seq.update root rootXCoord xcoords'')
  | otherwise = (leftEdge, leftEdge, xcoords)