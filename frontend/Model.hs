module Model where

import Miso
import Miso.String

import GraphReduction

data Model = Model
  { input :: MisoString
  , output :: Either String (Int, [Graph])
  , graphIndex :: Int
  }
  deriving (Eq)

data Action
  = Eval
  | Clear
  | TextInput MisoString
  | Next
  | Prev
  | NoOp
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { input = ""
  , output = Left ""
  , graphIndex = 0
  }