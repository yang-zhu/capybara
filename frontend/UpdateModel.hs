module UpdateModel(updateModel) where

import Miso
import Miso.String
import Control.Lens.Operators

import Lexer
import Parser
import GraphReduction as GR
import Model

turnBackslashIntoLambda :: MisoString -> MisoString
turnBackslashIntoLambda = toMisoString . Prelude.map (\c -> if c =='\\' then 'λ' else c) . fromMisoString

runBackend :: Model -> Model
runBackend model
  | model^.input == "" = model & output .~ Left ""
  | otherwise = case Parser.parse (fromMisoString (model^.input)) of
      Left err -> model & output .~ Left err
      Right ast -> model & output .~ Right (GR.run (model^.strategy) ast) & graphIndex .~ 0

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval model = noEff $ runBackend model
updateModel (TextInput newInput) model = noEff $ model & input .~ turnBackslashIntoLambda newInput
updateModel CBNeed model = noEff $ runBackend $ model & strategy .~ CallByNeed
updateModel CBName model = noEff $ runBackend $ model & strategy .~ CallByName
updateModel CBValue model = noEff $ runBackend $ model & strategy .~ CallByValue
updateModel Clear model = noEff $ model & input .~ "" & output .~ Left ""
updateModel Next model = noEff $ model & graphIndex %~ (+1)
updateModel Prev model = noEff $ model & graphIndex %~ subtract 1
updateModel NoOp model = noEff model