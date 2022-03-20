module Model where

import Control.Lens (makeLenses)
import Miso.String (MisoString)

import Parser
import GraphReduction


data Output = Output
  { _graph :: Maybe [(Maybe NodeIndex, Graph)]
  , _inputError :: Maybe ParseError
  , _definitions :: [Definition]
  }
  deriving Eq

makeLenses ''Output

data Model = Model
  { _termInput :: MisoString
  , _defInput :: MisoString
  , _strategy :: EvalStrategy
  , _output :: Output
  }
  deriving Eq

makeLenses ''Model

data Action
  = Eval
  | TermInput MisoString
  | DefInput MisoString
  | CBNeed
  | CBName
  | CBValue
  | Clear
  | Next
  | Prev
  | NoOp


initialModel :: Model
initialModel = Model
  { _termInput = ""
  , _defInput
      = "id = λx.x;\n\
        \True = λx.λy.x;\n\
        \False = λx.λy.y;\n\
        \not = λx.x False True;\n\
        \omega = (λx.(x x))(λx.(x x));\n\
        \fix = λf.(λx.(f (x x)))(λx.(f (x x)));\n\
        \S = λx.λy.λz.(x z)(y z);\n\
        \K = λx.λy.x;\n\
        \I = λx.x;\n"
  , _strategy = CallByNeed
  , _output = Output Nothing Nothing []
  }