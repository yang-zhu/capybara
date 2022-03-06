module Model where

import Miso
import Miso.String

import GraphReduction

data Model = Model
  { input :: MisoString
  , strategy :: EvalStrategy
  , output :: Either String (Int, [Graph])
  , graphIndex :: Int
  }
  deriving (Eq)

data Action
  = Eval
  | TextInput MisoString
  | CBNeed
  | CBName
  | CBValue
  | Clear
  | Next
  | Prev
  | NoOp
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { input = ""
  , strategy = CallByNeed
  , output = Left ""
  , graphIndex = 0
  }