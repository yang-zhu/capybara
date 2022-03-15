module Model where

import Control.Lens (makeLenses)
import Miso
import Miso.String

import Parser
import GraphReduction


data Output = Output
  { _graph :: Maybe (Int, [(Graph, Maybe Int)])
  , _inputError :: Maybe ParseError
  }
  deriving Eq

makeLenses ''Output

data Model = Model
  { _input :: MisoString
  , _definitions :: MisoString
  , _strategy :: EvalStrategy
  , _output :: Output
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
  , _output = Output Nothing Nothing
  , _graphIndex = 0
  }