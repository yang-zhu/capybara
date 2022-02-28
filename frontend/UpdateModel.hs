module UpdateModel(updateModel) where

import Miso
import Miso.String

import Lexer
import Parser
import GraphReduction as GR
import Model


runBackend :: Model -> Model
runBackend (Model input output) = case runParser abstraction (tokenize (fromMisoString input)) of
  Left err -> Model input (Left err)
  Right (ast, _) -> Model input (Right (GR.run ast))

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Eval m = noEff (runBackend m)
updateModel Clear m = noEff m{input="", output=Left ""}
updateModel (TextInput input) m = noEff (runBackend m{input=input})
updateModel NoOp m = noEff m