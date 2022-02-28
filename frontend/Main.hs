{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import           Miso
import Miso.String

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class

import Model
import UpdateModel
import ViewModel

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 3301 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp Miso.App {
    initialAction = NoOp,          -- initial action to be executed on application load
    model  = initialModel,                   -- initial model
    update = updateModel,          -- update function
    view   = viewModel,            -- view function
    events = defaultEvents,        -- default delegated events
    subs   = [],                   -- empty subscription list
    mountPoint = Nothing,          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)
  }
