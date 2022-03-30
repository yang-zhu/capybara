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
      = "S = λx y z. (x z)(y z);\n\
        \K = λx y. x;\n\
        \I = λx. x;\n\
        \omega = (λx.(x x))(λx.(x x));\n\
        \fix = λf.(λx.(f (x x)))(λx.(f (x x)));\n\
        \\n\
        \-- data Bool = True | False\n\
        \True = λt f. t;\n\
        \False = λt f. f;\n\
        \not = λb. b False True;\n\
        \and = λa b. a b False;\n\
        \or = λa b. a True b;\n\
        \\n\
        \-- data Nat = Zero | Succ Nat\n\
        \Zero = λz s.z;\n\
        \Succ = λn z s. s n;\n\
        \plus = λa b. a b (λa'. plus a' (Succ b));\n\
        \mult = λa b. a Zero (λa'. plus (mult a' b) b);\n\
        \\n\
        \-- data List a = Nil | Cons a (List a)\n\
        \Nil = λn c.n;\n\
        \Cons = λe es n c. c e es;\n\
        \append = λa b. a b (λe es. Cons e (append es b));\n\
        \nth = λl n. l omega (λe es. n e (λn'. nth es n'));\n"
  , _strategy = CallByNeed
  , _output = Output Nothing Nothing []
  }