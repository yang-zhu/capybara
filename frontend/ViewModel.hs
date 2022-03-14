module ViewModel(viewModel) where

import Data.Foldable(toList)
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Miso
import Miso.String
import Miso.Svg
import Control.Lens ((^.))

import GraphReduction
import Model


type Depth = Int
type XCoord = Double

header :: View Action
header = h1_ []
  [ img_ [src_ "logo.svg", alt_ "logo-handdrawn capybara"]
  , text "Capybara, a chilled λ-evaluator"
  ]

formButtons :: Model -> [View Action]
formButtons model =
  [ div_ [class_ "btn-group"]
      [ button_
        [ type_ "button", class_ "btn btn-success", onClick Eval ]
        [ text (stratToStr (model^.strategy)) ]
      , button_
        [ type_ "button"
        , class_ "btn btn-success dropdown-toggle dropdown-toggle-split"
        , textProp "data-bs-toggle" "dropdown"
        , textProp "aria-expanded" "false"
        ]
        [ span_ [class_ "visually-hidden"] [text "Toggle Dropdown"] ]
      , ul_ [class_ "dropdown-menu"]
        [ li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBNeed] [text (stratToStr CallByNeed)]]
        , li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBName] [text (stratToStr CallByName)]]
        , li_ []
          [Miso.a_ [class_"dropdown-item", href_ "#", onClick CBValue] [text (stratToStr CallByValue)]]
        ]
      ]
  , button_ [type_ "button", class_ "btn btn-secondary", onClick Clear] [text "clear"]
  ]
  where
    stratToStr :: EvalStrategy -> MisoString
    stratToStr CallByNeed = "call-by-need"
    stratToStr CallByName = "call-by-name"
    stratToStr CallByValue = "call-by-value"

onEnter :: Action -> Attribute Action
onEnter act = onKeyDown (hitEnter act)
  where
    hitEnter :: Action -> KeyCode -> Action
    hitEnter act (KeyCode 13) = act
    hitEnter _ _ = NoOp

inputArea :: Model -> View Action
inputArea model
  | Left err <- model^.output
  , err /= ""
  = div_ [Miso.id_ "input-area"]
    [ input_
      [ type_ "text"
      , class_ "form-control is-invalid"
      , placeholder_ "(\\x.x) y"
      , value_ (model^.input)
      , onInput TextInput
      , onEnter Eval
      , autofocus_ True
      ]
    , div_ [class_ "invalid-feedback"] [text (ms err)]
    ]
  | otherwise
  = input_
    [ type_ "text", class_ "form-control"
    , placeholder_ "(\\x.x) y"
    , value_ (model^.input)
    , onInput TextInput
    , onEnter Eval
    , autofocus_ True
    ]

form :: Model -> View Action
form m = div_ [Miso.id_ "form"] (inputArea m : formButtons m)

computeDepths :: (Int, Graph) -> Seq Depth -> Depth -> Seq Depth
computeDepths (root, graph) depths currDepth
  | currDepth > Seq.index depths root = case Seq.index (graph^.nodes) root of
      VarNode v -> depths'
      LamNode v e -> computeDepths (e, graph) depths' currDepth'
      AppNode e1 e2 -> let
        depths'' = computeDepths (e1, graph) depths' currDepth'
        in computeDepths (e2, graph) depths'' currDepth'
  | otherwise = depths
  where
    depths' = Seq.update root currDepth depths 
    currDepth' = currDepth + 1

layout :: (Int, Graph) -> Seq XCoord -> [XCoord] -> ([XCoord], XCoord, Seq XCoord)
layout (root, graph) xcoords (leftEdge0:leftEdge1:leftEdges)  -- [XCoord] is an infinite list
  | Seq.index xcoords root == -1 = case Seq.index (graph^.nodes) root of
      VarNode v -> ((leftEdge0+4):leftEdge1:leftEdges, leftEdge0+2, Seq.update root (leftEdge0+2) xcoords)  -- (right edge, root, xcoords)
      LamNode v e -> let
        (eRightEdges, eRoot, xcoords') = layout (e, graph) xcoords (max leftEdge0 leftEdge1 : leftEdges)
        in ((eRoot+2):eRightEdges, eRoot, Seq.update root eRoot xcoords')
      AppNode e1 e2 
        | e1 == e2 -> let
            (eRightEdges, eRoot, xcoords') = layout (e1, graph) xcoords (max leftEdge0 leftEdge1 : leftEdges)
            in ((eRoot+2):eRightEdges, eRoot, Seq.update root eRoot xcoords')
        | otherwise -> let
            (e1RightEdges, e1Root, xcoords') = layout (e1, graph) xcoords (max (leftEdge0-2) leftEdge1:leftEdges)
            (e2RightEdges, e2Root, xcoords'') = layout (e2, graph) xcoords' e1RightEdges
            rootXCoord = (e1Root + e2Root) / 2
            in ((rootXCoord+2):e2RightEdges, rootXCoord, Seq.update root rootXCoord xcoords'')
  | otherwise = ((leftEdge0+4):leftEdge1:leftEdges, leftEdge0+2, xcoords)
layout _ _ _ = undefined

xScale = 10
yScale = 40

draw :: (Int, (Graph, Maybe Int)) -> Seq Depth -> Seq XCoord -> [View Action]
draw (root, (graph, redex)) depths xcoords = case Seq.index (graph^.nodes) root of
  VarNode v -> [text_ [textAnchor_ "middle", x_ (ms rootX), y_ (ms rootY),fill_ "black"] [text (ms v)]]
  LamNode v e -> let
    eX = xScale * Seq.index xcoords e
    eY = yScale * Seq.index depths e
    in  [ text_
          [ dominantBaseline_ "auto"
          , textAnchor_ "middle", x_ (ms rootX)
          , y_ (ms rootY)
          ]
          [ text ("λ"<> ms v) ]
        , line_
          [ x1_ (ms rootX)
          , x2_ (ms eX)
          , y1_ (ms (rootY+5))
          , y2_ (ms (eY-15))
          , stroke_ "black"
          , strokeWidth_ "1.5"
          ] []
        ]
        ++ draw (e, (graph, redex)) depths xcoords
  AppNode e1 e2 -> let
    e1X = xScale * Seq.index xcoords e1
    e1Y = yScale * Seq.index depths e1
    e2X = xScale * Seq.index xcoords e2
    e2Y = yScale * Seq.index depths e2
    in  [ text_ 
          [ dominantBaseline_ "auto"
          , textAnchor_ "middle"
          , x_ (ms rootX)
          , y_ (ms rootY)
          , if Just root == redex then fontWeight_ "bolder" else fontWeight_ "normal"
          , if Just root == redex then fill_ "#0d6efd" else fill_ "black"
          ]
          [ text "@" ]
        , path_
          [ d_ 
            (  "M " <> ms (rootX-5) <> " " <> ms (rootY+5)
            <> "Q " <> ms (rootX-10) <> " " <> ms (rootY+10)
            <> " " <> ms e1X <> " " <> ms (e1Y-15)
            )
          , fill_ "transparent"
          , stroke_ "black"
          , strokeWidth_ "1.5"
          ] []
        , path_ 
          [ d_
            (  "M " <> ms (rootX+5)  <> " " <> ms (rootY+5)
            <> "Q " <> ms (rootX+10) <> " " <> ms (rootY+10)
            <> " " <> ms e2X <> " " <> ms (e2Y-15)
            )
          , fill_ "transparent"
          , stroke_ "black"
          , strokeWidth_ "1.5"
          ] []
        ]
       ++ draw (e1, (graph, redex)) depths xcoords
       ++ draw (e2, (graph, redex)) depths xcoords
  where
    rootX = xScale * Seq.index xcoords root
    rootY = yScale * Seq.index depths root

renderGraph :: Either String (Int, [(Graph, Maybe Int)]) -> Int -> View Action
renderGraph (Right (root, graphs)) index = let
  (graph, redex) = graphs !! index
  depths = computeDepths (root, graph) (Seq.replicate (Seq.length (graph^.nodes)) (-1)) 0
  (rightEdges, _, xcoords) = layout (root, graph) (Seq.replicate (Seq.length (graph^.nodes)) (-1)) (repeat 0)
  viewBoxWidth = xScale * Prelude.maximum (Prelude.take (Seq.length (graph^.nodes)) rightEdges) + 30
  viewBoxHeight = yScale * Prelude.maximum depths + 30
  in svg_
     [ Miso.Svg.width_ (ms (1.5 * viewBoxWidth))
     , Miso.Svg.height_ (ms (1.5 * fromIntegral viewBoxHeight :: Double))
     , viewBox_ ("-15 -15 " <> ms viewBoxWidth <> " " <> ms viewBoxHeight)
     ]
     (draw (root, (graph, redex)) depths xcoords)
renderGraph (Left err) _ = text ""

graphButtons :: Model -> View Action
graphButtons model
  | Right (_, graphs) <- model^.output
  = div_ [Miso.id_ "graph-buttons"]
    [ div_ [class_ "btn-group"]
      [ button_
        [ type_ "button"
        , class_ "btn btn-outline-primary"
        , disabled_ (model^.graphIndex == 0)
        , onClick Prev
        ]
        [ text "◀" ]
      , button_
        [ type_ "button"
        , class_ "btn btn-outline-primary"
        , disabled_ (model^.graphIndex == Prelude.length graphs - 1)
        , onClick Next
        ]
        [ text "▶" ]
      ]
    ]
  | otherwise = text ""

defButton :: View Action
defButton = div_ [Miso.id_ "def-btn-container"] 
  [ button_
    [ type_ "button"
    , class_ "btn btn-outline-primary"
    , textProp "data-bs-toggle" "collapse"
    , textProp "data-bs-target" "#definitions"
    , textProp "aria-expanded" "false"
    , textProp "aria-controls" "definitions"
    ]
    [ text "Definitions" ]
  ]

defInput :: Model -> View Action     
defInput model = div_ [class_ "collapse collapse-horizontal show", Miso.id_ "definitions"] 
  [ div_ [textProp "style" "width:300px"]
    [ textarea_
      [ type_ "text"
      , class_ "form-control"
      , rows_ "15"
      , textProp "style" "width:300px"
      , onInput DefInput
      , value_ (model^.definitions)
      ] []
    ]
  ]

graphView :: Model -> View Action
graphView model =
  div_ [Miso.id_ "graph"]
  [ graphButtons model
  , renderGraph (model^.output) (model^.graphIndex)
  ]

defAndGraph :: Model -> View Action
defAndGraph model = div_ [Miso.id_ "def-graph-container"] [defButton, defInput model, graphView model]

controlBar :: Model -> View Action
controlBar model
  = div_ [Miso.id_ "control-bar"][form model]

viewModel :: Model -> View Action
viewModel model
  = div_ []
    [ header
    , controlBar model
    , defAndGraph model
    ]