{-# LANGUAGE CPP #-}

module Main where

import Miso
import Miso.String

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import           Network.Wai.Application.Static
#endif
import Control.Monad.IO.Class

import Model
import UpdateModel
import ViewModel

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 3302 (f >> syncPoint) (staticApp (defaultWebAppSettings "static"))
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- Adapted from the sample project of miso https://github.com/dmjio/miso/blob/master/sample-app/Main.hs
main :: IO ()
main = runApp $ startApp Miso.App {
    initialAction = NoOp,
    model  = initialModel,
    update = updateModel,
    view   = viewModel,
    events = defaultEvents,
    subs   = [],
    mountPoint = Nothing,
    logLevel = Off
  }
