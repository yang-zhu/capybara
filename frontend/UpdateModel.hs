module UpdateModel(updateModel) where

import Miso
import Miso.String

import Lexer
import Parser
import GraphReduction as GR
import Model


runBackend :: Model -> Model
runBackend (Model input output index) = case runParser abstraction (tokenize (fromMisoString input)) of
  Left err -> Model input (Left err) index
  Right (ast, _) -> Model input (Right (GR.run ast)) index

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval m = noEff (runBackend m)
updateModel Clear m = noEff m{input="", output=Left ""}
updateModel (TextInput input) m = noEff (m{input=input})
updateModel Next m@Model{graphIndex} = noEff m{graphIndex=graphIndex+1}
updateModel Prev m@Model{graphIndex} = noEff m{graphIndex=graphIndex-1}
updateModel NoOp m = noEff m