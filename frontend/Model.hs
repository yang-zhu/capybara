module Model where

import Miso
import Miso.String
import Control.Lens (makeLenses)

import GraphReduction

data Model = Model
  { _input :: MisoString
  , _definitions :: MisoString
  , _strategy :: EvalStrategy
  , _output :: Either String (Int, [(Graph, Maybe Int)])
  , _graphIndex :: Int
  }
  deriving Eq

makeLenses ''Model

data Action
  = Eval
  | TextInput MisoString
  | DefInput MisoString
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
  { _input = ""
  , _definitions
      = "True = λx.λy.x;\n\
        \False = λx.λy.y;\n\
        \not = λx.x False True;\n"
  , _strategy = CallByNeed
  , _output = Left ""
  , _graphIndex = 0
  }