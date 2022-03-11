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
header = h1_ []
  [ img_ [src_ "logo.svg", alt_ "logo-handdrawn capybara"]
  , text "Capybara - chilled evaluation" ]

formButtons :: [View Action]
formButtons =
  [ div_ [class_ "btn-group"]
      [ button_ [type_ "button", class_ "btn btn-success", onClick Eval] [text "evaluate"]
      , button_ [type_ "button", class_ "btn btn-success dropdown-toggle dropdown-toggle-split", textProp "data-bs-toggle" "dropdown", textProp "aria-expanded" "false"] [span_ [class_ "visually-hidden"] [text "Toggle Dropdown"]]
      , ul_ [class_ "dropdown-menu"]
        [ li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBNeed] [text "call-by-need"]]
        , li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBName] [text "call-by-name"]]
        , li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBValue] [text "call-by-value"]]
        ]
      ]
  , button_ [type_ "button", class_ "btn btn-secondary", onClick Clear] [text "clear"]
  ]

onEnter :: Action -> Attribute Action
onEnter act = onKeyDown (hitEnter act)
  where
    hitEnter :: Action -> KeyCode -> Action
    hitEnter act (KeyCode 13) = act
    hitEnter _ _ = NoOp

inputArea :: Model -> View Action
inputArea Model{input, output=Left err}
 | err /= "" = div_ [Miso.id_ "input-area"]
    [ input_ [type_ "text", class_ "form-control is-invalid", placeholder_ "(\\x.x) y", value_ input, onInput TextInput, onEnter Eval, autofocus_ True]
    , div_ [class_ "invalid-feedback"] [text (ms err)]
    ]
inputArea Model{input} = input_ [type_ "text", class_ "form-control", placeholder_ "(\\x.x) y", value_ input, onInput TextInput, onEnter Eval, autofocus_ True]

form :: Model -> View Action
form m = div_ [Miso.id_ "form"] (inputArea m : formButtons)

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
      VarNode v -> (leftEdge+4, leftEdge+2, Seq.update root (leftEdge+2) xcoords)  -- (right edge, root, xcoords)
      LamNode v e -> let
        (eRightEdge, eRoot, xcoords') = layout (e, graph) xcoords leftEdge
        in (eRightEdge, eRoot, Seq.update root eRoot xcoords')
      AppNode e1 e2 -> let
        (e1RightEdge, e1Root, xcoords') = layout (e1, graph) xcoords leftEdge
        (e2RightEdge, e2Root, xcoords'') = layout (e2, graph) xcoords' e1RightEdge
        rootXCoord = (e1Root + e2Root) / 2
        in (e2RightEdge, rootXCoord, Seq.update root rootXCoord xcoords'')
  | otherwise = (leftEdge, leftEdge, xcoords)

xScale = 10
yScale = 40

draw :: (Int, (Graph, Maybe Int)) -> Seq Depth -> Seq XCoord -> [View Action]
draw (root, (graph, redex)) depths xcoords = case Seq.index graph root of
  VarNode v -> [text_ [textAnchor_ "middle", x_ (ms rootX), y_ (ms rootY),fill_ "black"] [text (ms v)]]
  LamNode v e -> let
    eX = xScale * Seq.index xcoords e
    eY = yScale * Seq.index depths e
    in [ text_ [dominantBaseline_ "auto", textAnchor_ "middle", x_ (ms rootX), y_ (ms rootY)] [text ("λ"<> ms v)]
       , line_ [x1_ (ms rootX), x2_ (ms eX), y1_ (ms (rootY+5)), y2_ (ms (eY-15)), stroke_ "black", strokeWidth_ "1.5"] []
       ] ++ draw (e, (graph, redex)) depths xcoords
  AppNode e1 e2 -> let
    e1X = xScale * Seq.index xcoords e1
    e1Y = yScale * Seq.index depths e1
    e2X = xScale * Seq.index xcoords e2
    e2Y = yScale * Seq.index depths e2
    in [ text_ 
          [ dominantBaseline_ "auto"
          , textAnchor_ "middle"
          , x_ (ms rootX)
          , y_ (ms rootY)
          , if Just root == redex then fontWeight_ "bolder" else fontWeight_ "normal"
          , if Just root == redex then fill_ "#0d6efd" else fill_ "black"]
          [text "@"]
        , path_ [d_ ("M " <> ms (rootX-5) <> " " <> ms (rootY+5) <> "Q " <> ms (rootX-10) <> " " <> ms (rootY+10) <> " " <> ms e1X <> " " <> ms (e1Y-15)), fill_ "transparent", stroke_ "black", strokeWidth_ "1.5"] []
        , path_ [d_ ("M " <> ms (rootX+5) <> " " <> ms (rootY+5) <> "Q " <> ms (rootX+10) <> " " <> ms (rootY+10) <> " " <> ms e2X <> " " <> ms (e2Y-15)), fill_ "transparent", stroke_ "black", strokeWidth_ "1.5"] []
        ] ++ draw (e1, (graph, redex)) depths xcoords ++ draw (e2, (graph, redex)) depths xcoords
  where
    rootX = xScale * Seq.index xcoords root
    rootY = yScale * Seq.index depths root

-- markRedex :: Maybe Int -> Seq Depth -> Seq XCoord -> View Action
-- markRedex (Just idx) depths xcoords = let
--   rectX = 10 * Seq.index xcoords idx
--   rectY = 50 * Seq.index depths idx - 4
--   in circle_ [cx_ (ms rectX), cy_ (ms rectY), r_ "10", fill_ "orange", stroke_ "transparent"] []
-- markRedex Nothing _ _ = text ""

renderGraph :: Either String (Int, [(Graph, Maybe Int)]) -> Int -> View Action
renderGraph (Right (root, graphs)) index = let
  (graph, redex) = graphs !! index
  depths = computeDepths (root, graph) (Seq.replicate (Seq.length graph) (-1)) 0
  (rightEdge, _, xcoords) = layout (root, graph) (Seq.replicate (Seq.length graph) (-1)) 0
  viewBoxWidth = xScale * rightEdge + 30
  viewBoxHeight = yScale * Prelude.maximum depths + 30
  in svg_ [Miso.Svg.width_ (ms (1.5 * viewBoxWidth)), Miso.Svg.height_ (ms (1.5 * fromIntegral viewBoxHeight :: Double)), viewBox_ ("-15 -15 " <> ms viewBoxWidth <> " " <> ms viewBoxHeight)]
          -- (markRedex redex depths xcoords: draw (root, graph) depths xcoords)
          (draw (root, (graph, redex)) depths xcoords)
renderGraph (Left err) _ = text ""

graphButtons :: Model -> View Action
graphButtons Model{output=Right (_, graphs), graphIndex=gi} = div_ [Miso.id_ "graph-buttons"] 
  [ div_ [class_ "btn-group"]
      [ button_ [type_ "button", class_ "btn btn-outline-primary", disabled_ (gi == 0), onClick Prev] [text "◀"]
      , button_ [type_ "button", class_ "btn btn-outline-primary", disabled_ (gi == Prelude.length graphs - 1), onClick Next] [text "▶"]
      ]
  ]
graphButtons m = text ""

graphView :: Model -> View Action
graphView Model{output, graphIndex} = div_ [Miso.id_ "graph"]
  [ renderGraph output graphIndex
  ]

controlBar :: Model -> View Action
controlBar m = div_ [Miso.id_ "control-bar"]
  [ form m
  , graphButtons m
  ]

viewModel :: Model -> View Action
viewModel m@(Model input strat output index) = div_ []
  [ header
  , controlBar m
  , graphView m
  ]