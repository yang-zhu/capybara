module Model where

import Miso
import Miso.String

import GraphReduction

data Model = Model
  { input :: MisoString
  , output :: Either String [Graph]
  }
  deriving (Eq)

data Action
  = Eval
  | Clear
  | TextInput MisoString
  | NoOp
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { input = ""
  , output = Left ""
  }