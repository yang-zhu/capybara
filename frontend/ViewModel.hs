module ViewModel(viewModel) where

import Data.Foldable(toList)
import Miso
import Miso.String

import GraphReduction
import Model

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