-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import Miso.String

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
import Data.Foldable(toList)

import Lexer
import Parser
import GraphReduction as GR

-- | Type synonym for an application model
data Model = Model
  { input :: MisoString
  , output :: Either String [Graph]
  }
  deriving (Eq)

-- | Sum type for application events
data Action
  = Eval
  | Clear
  | TextInput MisoString
  | NoOp
  deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp Miso.App {
    initialAction = NoOp,          -- initial action to be executed on application load
    model  = Model "" (Left ""),                   -- initial model
    update = updateModel,          -- update function
    view   = viewModel,            -- view function
    events = defaultEvents,        -- default delegated events
    subs   = [],                   -- empty subscription list
    mountPoint = Nothing,          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off }               -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

runBackend :: Model -> Model
runBackend (Model input output) = case runParser abstraction (tokenize (fromMisoString input)) of
  Left err -> Model input (Left err)
  Right (ast, _) -> Model input (Right (GR.run ast))

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Eval m = noEff (runBackend m)
updateModel Clear m = noEff m{input="", output=Left ""}
updateModel (TextInput input) m = noEff (runBackend m{input=input})
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel (Model input output) = div_ [] [
  header,
  input_ [type_ "text", placeholder_ "(\\x.x) y", value_ input, onInput TextInput],
  button_ [onClick Eval] [text "evaluate"],
  button_ [onClick Clear] [text "clear"],
  div_ [] [renderGraph output]
 ]

header :: View Action
header = h1_ [] [text "Capy-Lambda" ]

renderGraph :: Either String [Graph] -> View Action
renderGraph (Right graphs) = ul_ [] [li_ [] [(text . ms . show .toList) graph] | graph <- graphs]
renderGraph (Left err) = text (ms err)
