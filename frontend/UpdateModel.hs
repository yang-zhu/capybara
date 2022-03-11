module UpdateModel(updateModel) where

import Miso
import Miso.String

import Lexer
import Parser
import GraphReduction as GR
import Model

turnBackslashIntoLambda :: MisoString -> MisoString
turnBackslashIntoLambda = toMisoString . Prelude.map (\c -> if c =='\\' then 'λ' else c) . fromMisoString

runBackend :: Model -> Model
runBackend m@Model{input, strategy, output} = case Parser.parse (fromMisoString input) of
  Left err -> m{output=Left err}
  Right ast -> m{output=Right (GR.run strategy ast)}

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval m = noEff (runBackend m)
updateModel (TextInput input) m = noEff (m{input=turnBackslashIntoLambda input})
updateModel CBNeed m = noEff (runBackend m{strategy=CallByNeed})
updateModel CBName m = noEff (runBackend m{strategy=CallByName})
updateModel CBValue m = noEff (runBackend m{strategy=CallByValue})
updateModel Clear m = noEff m{input="", output=Left ""}
updateModel Next m@Model{graphIndex} = noEff m{graphIndex=graphIndex+1}
updateModel Prev m@Model{graphIndex} = noEff m{graphIndex=graphIndex-1}
updateModel NoOp m = noEff m