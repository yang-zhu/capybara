module ViewModel(viewModel) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Lens ((^.), (^?), (^?!), _1, _2, _Just, _head)
import Miso
import Miso.String (MisoString, ms)
import Miso.Svg

import Lexer
import Parser
import GraphReduction
import Model


type Depth = Int
type XCoord = Double


header :: View Action
header =
  h4_
    []
    [ img_ [src_ "logo.svg", alt_ "logo-handdrawn capybara"]
    , text "Capybara "
    , small_
        [class_ "text-muted", textProp "style" "margin-left: 0.5em"]
        [text "a chilled λ-evaluator"]
    ]

defMenu :: View Action
defMenu =
  div_
    []
    [button_
      [ type_ "button"
      , class_ "btn btn-outline-success material-icons"
      , textProp "data-bs-toggle" "collapse"
      , textProp "data-bs-target" "#definitions"
      , textProp "aria-expanded" "false"
      , textProp "aria-controls" "definitions"
      , textProp "style" "padding: 3px; font-size: 25px;"
      ]
      [ text "reorder" ]
    ]

formButtons :: Model -> [View Action]
formButtons model =
  [ div_
    [class_ "btn-group"]
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
  , button_
      [ type_ "button"
      , class_ "btn btn-secondary"
      , onClick Clear
      ]
      [ text "clear" ]
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
inputArea model =
  div_
    [ Miso.id_ "input-area" ]
    [ input_
      [ type_ "text"
      , if isJust $ model ^? output . inputError . _Just . _ExprError
          then class_ "form-control is-invalid"
          else class_ "form-control"
      , placeholder_ "(\\x. x) y"
      , value_ (model^.termInput)
      , onInput TermInput
      , onEnter Eval
      , autofocus_ True
      ]
    ]

form :: Model -> View Action
form m =
  div_
    [Miso.id_ "form"]
    (defMenu : inputArea m : formButtons m)

computeDepths :: Graph -> Int -> Seq Depth -> Depth -> Seq Depth
computeDepths graph root depths currDepth
  | currDepth > Seq.index depths root = case Seq.index (graph^.nodes) root of
      VarNode v -> depths'
      LamNode v e -> computeDepths graph e depths' currDepth'
      AppNode e1 e2 -> let
        depths'' = computeDepths graph e1 depths' currDepth'
        in computeDepths graph e2 depths'' currDepth'
  | otherwise = depths
  where
    depths' = Seq.update root currDepth depths
    currDepth' = currDepth + 1

-- generated using https://bl.ocks.org/tophtucker/62f93a4658387bb61e4510c37e2e97cf
letterWidthTable = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.2490234375,0.33359375,0.40859375,0.5,0.5,0.83359375,0.778125,0.18046875,0.33359375,0.33359375,0.5,0.5640625,0.25,0.33359375,0.25,0.278125,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.278125,0.278125,0.5640625,0.5640625,0.5640625,0.44453125,0.92109375,0.72265625,0.6671875,0.6671875,0.72265625,0.6109375,0.55625,0.72265625,0.72265625,0.33359375,0.38984375,0.72265625,0.6109375,0.88984375,0.72265625,0.72265625,0.55625,0.72265625,0.6671875,0.55625,0.6109375,0.72265625,0.72265625,0.94453125,0.72265625,0.72265625,0.6109375,0.33359375,0.33984375,0.33359375,0.46953125,0.5,0.343359375,0.44453125,0.5,0.44453125,0.5,0.44453125,0.3828125,0.5,0.5,0.278125,0.3357421875,0.5,0.278125,0.778125,0.5,0.5,0.5,0.5,0.33642578125,0.38984375,0.27880859375,0.5,0.5,0.72265625,0.5,0.5,0.44453125,0.48046875,0.20078125,0.48046875,0.54140625]
defaultLetterWidth = 0.5122738486842104

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:xs) !? i
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

stringWidth :: String -> Double
stringWidth s = 2 * foldl (\acc c -> charWidth c + acc) 0 s
  where
    charWidth :: Char -> Double
    charWidth c = fromMaybe defaultLetterWidth (letterWidthTable !? fromEnum c)

layout :: Graph -> Int -> Seq XCoord -> [XCoord] -> ([XCoord], XCoord, Seq XCoord)
layout graph root xcoords (leftEdge0:leftEdge1:leftEdges)  -- [XCoord] is an infinite list
  | Seq.index xcoords root == -1 = case Seq.index (graph^.nodes) root of
      VarNode v -> let
        nodeWidth = max 4 (stringWidth v)
        in ((leftEdge0+nodeWidth):leftEdge1:leftEdges, leftEdge0+nodeWidth/2, Seq.update root (leftEdge0+nodeWidth/2) xcoords)  -- (right edges, root, xcoords)
      LamNode v e -> let
        (eRightEdges, eRoot, xcoords') = layout graph e xcoords (max leftEdge0 leftEdge1 : leftEdges)
        in ((eRoot+2):eRightEdges, eRoot, Seq.update root eRoot xcoords')
      AppNode e1 e2
        | e1 == e2 -> let
            (eRightEdges, eRoot, xcoords') = layout graph e1 xcoords (max leftEdge0 leftEdge1 : leftEdges)
            in ((eRoot+2):eRightEdges, eRoot, Seq.update root eRoot xcoords')
        | otherwise -> let
            (e1RightEdges, e1Root, xcoords') = layout graph e1 xcoords (max (leftEdge0-2) leftEdge1:leftEdges)
            (e2RightEdges, e2Root, xcoords'') = layout graph e2 xcoords' e1RightEdges
            rootXCoord = (e1Root + e2Root) / 2
            in ((rootXCoord+2):e2RightEdges, rootXCoord, Seq.update root rootXCoord xcoords'')
  | otherwise = ((leftEdge0+4):leftEdge1:leftEdges, leftEdge0+2, xcoords)
layout _ _ _ _ = undefined

xScale = 10
yScale = 40

draw :: (Maybe Int, Graph) -> Int -> Seq Depth -> Seq XCoord -> [View Action]
draw (redex, graph) root depths xcoords = case Seq.index (graph^.nodes) root of
  VarNode v ->
    [ text_
        [ textAnchor_ "middle"
        , x_ (ms rootX)
        , y_ (ms rootY)
        , if Just root == redex then fontWeight_ "bolder" else fontWeight_ "normal"
        , if Just root == redex then fill_ "#0d6efd" else fill_ "black"
        ]
        [ text (ms v) ]
    ]
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
            ]
            []
        ]
        ++ draw (redex, graph) e depths xcoords
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
            ]
            []
        , path_
            [ d_
              (  "M " <> ms (rootX+5)  <> " " <> ms (rootY+5)
              <> "Q " <> ms (rootX+10) <> " " <> ms (rootY+10)
              <> " " <> ms e2X <> " " <> ms (e2Y-15)
              )
            , fill_ "transparent"
            , stroke_ "black"
            , strokeWidth_ "1.5"
            ]
            []
        ]
        ++ draw (redex, graph) e1 depths xcoords
        ++ draw (redex, graph) e2 depths xcoords
  where
    rootX = xScale * Seq.index xcoords root
    rootY = yScale * Seq.index depths root

renderGraph :: [(Maybe Int, Graph)] -> View Action
renderGraph (graph1:graph2:graphs) = let
  (redex, _) = graph1
  (_, graph) = graph2
  root = graph ^. rootIndex
  depths = computeDepths graph root (Seq.replicate (Seq.length (graph^.nodes)) (-1)) 0
  (rightEdges, _, xcoords) = layout graph root (Seq.replicate (Seq.length (graph^.nodes)) (-1)) (repeat 0)
  viewBoxWidth = xScale * maximum (take (Seq.length (graph^.nodes)) rightEdges) + 30
  viewBoxHeight = yScale * maximum depths + 30
  in svg_
      [ Miso.Svg.width_ (ms (1.5 * viewBoxWidth))
      , Miso.Svg.height_ (ms (1.5 * fromIntegral viewBoxHeight :: Double))
      , viewBox_ ("-15 -15 " <> ms viewBoxWidth <> " " <> ms viewBoxHeight)
      ]
      (draw (redex, graph) root depths xcoords)
renderGraph _ = undefined

graphButtons :: Model -> View Action
graphButtons model
  | Just graphs <- model ^. (output . graph) =
    div_
      [ Miso.id_ "graph-buttons" ]
      [ div_
          [class_ "btn-group"]
          [ button_
              [ type_ "button"
              , class_ "btn btn-outline-primary"
              , disabled_ (null $ drop 2 $ model ^?! (output . graph . _Just))
              , onClick Prev
              ]
              [ text "◀" ]
          , button_
              [ type_ "button"
              , class_ "btn btn-outline-primary"
              , disabled_ (isNothing $ model ^?! (output . graph . _Just . _head . _1))
              , onClick Next
              ]
              [ text "▶" ]
          ]
      ]
  | otherwise = text ""

defArea :: Model -> View Action
defArea model =
  div_
    [ class_ "collapse collapse-horizontal"
    , Miso.id_ "definitions"
    ]
    [ div_
        [ class_ "card"
        , textProp "style" "width:300px"
        ]
        [ div_
            [class_ "card-header"]
            [text "Definitions"]
        , div_
            [ class_ "card-body"
            , textProp "style" "padding:0"
            ]
            [ textarea_
                ((if isJust $ model ^? output . inputError . _Just . _DefError
                    then [class_ "form-control is-invalid"]
                    else [class_ "form-control", textProp "style" "border:0"])
                ++  [ type_ "text"
                    , rows_ "15"
                    , onInput DefInput
                    , value_ (model^.defInput)
                    ])
                []
            ]
        ]
    ]

composeErrorMsg :: (String, TokenWithPos) -> Bool -> String
composeErrorMsg (msg, tok@(InvalidToken c, _, _)) isDefError = "Invalid character " ++ show c ++ " at position " ++ showPosition tok isDefError ++ "."
composeErrorMsg (msg, (EndOfInput, _, _)) _ = msg ++ ", but found end of input."
composeErrorMsg (msg, tok@(t, _, _)) isDefError = msg ++ ", but found " ++ show t ++ " at position " ++ showPosition tok isDefError ++ "."

graphView :: Model -> View Action
graphView model
  | Just graphs <- model ^. (output . graph) =
    div_
      [ Miso.id_ "graph-area" ]
      [ graphButtons model
      , renderGraph graphs
      ]
  | Just (ExprError err) <- model ^. (output . inputError) =
    div_
      [ Miso.id_ "graph-area" ]
      [ div_
        [class_ "alert alert-danger"]
        [text (ms (composeErrorMsg err False))]
      ]
  | Just (DefError err) <- model ^. (output . inputError) =
    div_
      [ Miso.id_ "graph-area" ]
      [ div_
        [class_ "alert alert-danger"]
        [text (ms (composeErrorMsg err True))]]
  | otherwise = text ""

defAndGraph :: Model -> View Action
defAndGraph model =
  div_
    [ Miso.id_ "def-graph-container" ]
    [ div_ [] []  -- empty div to make collapse work
    , defArea model
    , graphView model
    ]

viewModel :: Model -> View Action
viewModel model =
  div_
    []
    [ header
    , form model
    , defAndGraph model
    ]