module ViewModel(viewModel) where

import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State (execState, runState)
import Control.Lens ((^.), (^?), (^?!), _1, _2, _Just, _head)
import Miso
import Miso.String (MisoString, ms)
import Miso.Svg hiding (id_, a_)

import Lexer
import Parser
import GraphReduction
import Model
import Layout


-- | The header includes a logo image and the title.
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

-- | The button that reveals the text area for definitions.
defMenu :: View Action
defMenu =
  div_
    []
    [ button_
        [ type_ "button"
        , class_ "btn btn-outline-success material-icons-round"
        , textProp "data-bs-toggle" "collapse"
        , textProp "data-bs-target" "#definitions"
        , textProp "aria-expanded" "false"
        , textProp "aria-controls" "definitions"
        , textProp "style" "padding: 3px; font-size: 25px;"
        ]
        [ text "reorder" ]
    ]

-- | One button triggers the evaluation and the other one clears the input.
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
      -- the button for evaluation has a dropdown menu that contains the three evaluation strategies to choose from
    , ul_
        [class_ "dropdown-menu"]
        [ li_
            []
            [ a_
                [class_"dropdown-item", href_ "#", onClick CBNeed]
                [text $ stratToStr CallByNeed]
            ]
        , li_
            []
            [ a_
                [class_"dropdown-item", href_ "#", onClick CBName]
                [text (stratToStr CallByName)]
            ]
        , li_
            []
            [ a_
                [class_"dropdown-item", href_ "#", onClick CBValue]
                [text (stratToStr CallByValue)]
            ]
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

-- | Triggers the act action when the enter key is pressed.
onEnter :: Action -> Attribute Action
onEnter act = onKeyDown hitEnter
  where
    hitEnter :: KeyCode -> Action
    hitEnter (KeyCode 13) = act
    hitEnter _ = NoOp

-- | The area for the input term.
termArea :: Model -> View Action
termArea model =
  div_
    [ id_ "term-area" ]
    [ input_
      [ type_ "text"
      , id_ "term-input"
      , if isJust $ model ^? output . inputError . _Just . _ExprError
          then class_ "form-control is-invalid"
          else class_ "form-control"
      , placeholder_ "(\\x.x) y"
      , value_ (model^.termInput)
      , onInput TermInput
      , onEnter Eval
      , autofocus_ True
      ]
    ]

form :: Model -> View Action
form m =
  div_
    [id_ "form"]
    (defMenu : termArea m : formButtons m)

xScale = 10
yScale = 40
belowTextXDiff = 5
belowTextYDiff = 5
aboveTextYDiff = 15

draw :: (Maybe NodeIndex, Graph) -> NodeIndex -> Seq Depth -> Seq XCoord -> [View Action]
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
            , y1_ (ms (rootY+belowTextYDiff))
            , y2_ (ms (eY-aboveTextYDiff))
            , stroke_ "black"
            , strokeWidth_ "1.5"
            , strokeLinecap_ "round"
            ]
            []
        ]
        ++ draw (redex, graph) e depths xcoords
  AppNode e1 e2 -> let
    e1X = xScale * Seq.index xcoords e1
    e1Y = yScale * Seq.index depths e1
    e2X = xScale * Seq.index xcoords e2
    e2Y = yScale * Seq.index depths e2
    lCtlPtX = rootX - ctlPtXDiff e1Y (e1X >= rootX)
    rCtlPtX = rootX + ctlPtXDiff e2Y (e2X <= rootX)
    lCtlPtY = rootY + ctlPtYDiff e1Y (e1X >= rootX)
    rCtlPtY = rootY + ctlPtYDiff e2Y (e2X <= rootX)
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
              (  "M " <> ms (rootX-belowTextXDiff) <> " " <> ms (rootY+belowTextYDiff)
              <> "Q " <> ms lCtlPtX <> " " <> ms lCtlPtY
              <> " " <> ms e1X <> " " <> ms (e1Y-aboveTextYDiff)
              )
            , fill_ "transparent"
            , stroke_ "black"
            , strokeWidth_ "1.5"
            , strokeLinecap_ "round"
            ]
            []
        , path_
            [ d_
              (  "M " <> ms (rootX+belowTextXDiff)  <> " " <> ms (rootY+belowTextYDiff)
              <> "Q " <> ms rCtlPtX <> " " <> ms rCtlPtY
              <> " " <> ms e2X <> " " <> ms (e2Y-aboveTextYDiff)
              )
            , fill_ "transparent"
            , stroke_ "black"
            , strokeWidth_ "1.5"
            , strokeLinecap_ "round"
            ]
            []
        ]
        ++ draw (redex, graph) e1 depths xcoords
        ++ draw (redex, graph) e2 depths xcoords
  where
    rootX = xScale * Seq.index xcoords root
    rootY = yScale * Seq.index depths root

    -- Computes the x-coordinate of the control point.
    ctlPtXDiff :: Depth -> Bool -> Double
    ctlPtXDiff endPtY wrongSide
      | endPtY - rootY > yScale = fromIntegral yScale
      | endPtY - rootY == yScale && wrongSide = 15
      | otherwise = belowTextXDiff
    
    -- Computes the y-coordinate of the control point.
    ctlPtYDiff :: Depth -> Bool -> Int
    ctlPtYDiff endPtY wrongSide
      | endPtY - rootY > yScale = yScale
      | endPtY - rootY == yScale && wrongSide = 15
      | otherwise = belowTextYDiff

    -- belowTextXDiff :: XCoord -> Depth -> Double
    -- belowTextXDiff x y = if y - rootY > 1 then 5 else (x - rootX) / fromIntegral (y - rootY) * fromIntegral belowTextYDiff

-- | Given a graph, computes the position of its nodes and renders the graph.
renderGraph :: [(Maybe NodeIndex, Graph)] -> View Action
renderGraph (graph1:graph2:graphs) = let
  (redex, _) = graph1
  (_, graph) = graph2
  root = graph ^. rootIndex
  depths = execState (runReaderT (computeDepths root 0) graph) (Seq.replicate (Seq.length (graph^.nodes)) (-1))
  ((rightEdges, _), xcoords) = runState (runReaderT (runReaderT (layout root 0 (repeat 0)) graph) depths) (Seq.replicate (Seq.length (graph^.nodes)) (-1))
  viewBoxWidth = xScale * maximum (take (Seq.length (graph^.nodes)) rightEdges) + 30
  viewBoxHeight = yScale * maximum depths + 30
  in svg_
      [ Miso.Svg.width_ (ms (1.5 * viewBoxWidth))
      , viewBox_ ("-15 -15 " <> ms viewBoxWidth <> " " <> ms viewBoxHeight)
      ]
      (draw (redex, graph) root depths xcoords)
renderGraph _ = undefined

onArrows :: Action -> Action -> Attribute Action
onArrows act1 act2 = onKeyDown hitArrow
  where
    hitArrow :: KeyCode -> Action
    hitArrow (KeyCode 37) = act1
    hitArrow (KeyCode 39) = act2
    hitArrow _ = NoOp

-- | The buttons that trigger the Next and Prev actions.
graphButtons :: Model -> View Action
graphButtons model
  | Just graphs <- model ^. (output . graph) =
    div_
      [ id_ "graph-buttons"
      ]
      [ div_
          [ class_ "btn-group"
          ]
          [ button_
              [ id_ "prev-button"
              , type_ "button"
              , class_ "btn btn-outline-primary material-icons-round"
              , classList_ [("disabled", null $ drop 2 $ model ^?! (output . graph . _Just))]
              , onClick Prev
              , onArrows Prev Next
              ]
              [ text "navigate_before" ]
          , button_
              [ id_ "next-button"
              , type_ "button"
              , class_ "btn btn-outline-primary material-icons-round"
              , classList_ [("disabled", isNothing $ model ^?! (output . graph . _Just . _head . _1))]
              , onClick Next
              , onArrows Prev Next
              ]
              [ text "navigate_next" ]
          ]
      ]
  | otherwise = text ""

-- | The text area that contains all the definitions.
defArea :: Model -> View Action
defArea model =
  div_
    [ class_ "collapse collapse-horizontal"
    , id_ "definitions"
    ]
    [ div_
        [ class_ "card"
        , textProp "style" "width:300px"
        ]
        [ div_
            [ class_ "card-header" ]
            [ text "Definitions" ]
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

-- | Composes an error message out of the output of the parser.
composeErrorMsg :: (String, TokenWithPos) -> Bool -> String
composeErrorMsg (msg, tok@(InvalidToken c, _, _)) isDefError = "Invalid character " ++ show c ++ " at position " ++ showPosition tok isDefError ++ "."
composeErrorMsg (msg, (EndOfInput, _, _)) _ = msg ++ ", but found end of input."
composeErrorMsg (msg, tok@(t, _, _)) isDefError = msg ++ ", but found " ++ show t ++ " at position " ++ showPosition tok isDefError ++ "."

-- | This area can either display a graph or an error message.
graphView :: Model -> View Action
graphView model
  | Just graphs <- model ^. (output . graph) =
    div_
      [ id_ "graph-area" ]
      [ graphButtons model
      , renderGraph graphs
      ]
  | Just (ExprError err) <- model ^. (output . inputError) =
    div_
      [ id_ "graph-area" ]
      [ div_
        [class_ "alert alert-danger"]
        [text (ms (composeErrorMsg err False))]
      ]
  | Just (DefError err) <- model ^. (output . inputError) =
    div_
      [ id_ "graph-area" ]
      [ div_
        [class_ "alert alert-danger"]
        [text (ms (composeErrorMsg err True))]]
  | otherwise = text ""

defAndGraph :: Model -> View Action
defAndGraph model =
  div_
    [ id_ "def-graph-container" ]
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