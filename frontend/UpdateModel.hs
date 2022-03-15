module UpdateModel(updateModel) where

import Control.Lens ((&), (^.), (%~), (.~))
import Miso
import Miso.String

import Lexer
import Parser
import GraphReduction as GR
import Model


turnBackslashIntoLambda :: MisoString -> MisoString
turnBackslashIntoLambda = toMisoString . Prelude.map (\c -> if c =='\\' then 'λ' else c) . fromMisoString

runBackend :: Model -> Model
runBackend model
  | model^.input == "" = model & output .~ Output Nothing Nothing
  | otherwise = model & output .~ newOutput & graphIndex .~ 0
  where
    newOutput = case parseExpression (fromMisoString (model^.input)) of
      Left err -> Output Nothing (Just err)
      Right expr -> case parseDefinitions (fromMisoString (model^.definitions)) of
        Left err -> Output Nothing (Just err)
        Right defs -> Output (Just (GR.run (model^.strategy) expr defs)) Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval model = noEff $ runBackend model
updateModel (TextInput newInput) model = noEff $ model & input .~ turnBackslashIntoLambda newInput
updateModel (DefInput newInput) model = noEff $ model & definitions .~ turnBackslashIntoLambda newInput
updateModel CBNeed model = noEff $ runBackend $ model & strategy .~ CallByNeed
updateModel CBName model = noEff $ runBackend $ model & strategy .~ CallByName
updateModel CBValue model = noEff $ runBackend $ model & strategy .~ CallByValue
updateModel Clear model = noEff $ model & input .~ "" & output .~ Output Nothing Nothing
updateModel Next model = noEff $ model & graphIndex %~ (+1)
updateModel Prev model = noEff $ model & graphIndex %~ subtract 1
updateModel NoOp model = noEff model