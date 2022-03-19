module Main where

import Miso

import Model
import UpdateModel
import ViewModel


-- Adapted from the sample project of miso https://github.com/dmjio/miso/blob/master/sample-app/Main.hs
main :: IO ()
main = startApp Miso.App {
    initialAction = NoOp,
    model  = initialModel,
    update = updateModel,
    view   = viewModel,
    events = defaultEvents,
    subs   = [],
    mountPoint = Nothing,
    logLevel = Off
  }
