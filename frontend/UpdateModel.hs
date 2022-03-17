module UpdateModel(updateModel) where

import Control.Lens ((&), (^.), (%~), (.~), (^?!), _1, _2, _Just, _head)
import Miso
import Miso.String (MisoString, fromMisoString, toMisoString)

import Lexer
import Parser
import GraphReduction
import Model


turnBackslashIntoLambda :: MisoString -> MisoString
turnBackslashIntoLambda = toMisoString . Prelude.map (\c -> if c =='\\' then 'λ' else c) . fromMisoString

eval :: Model -> Model
eval model
  | model^.termInput == "" = model & output .~ Output Nothing Nothing []
  | otherwise = model & output .~ newOutput
  where
    newOutput = case parseExpression (fromMisoString (model^.termInput)) of
      Left err -> Output Nothing (Just err) []
      Right term -> case parseDefinitions (fromMisoString (model^.defInput)) of
        Left err -> Output Nothing (Just err) []
        Right defs -> Output (Just (firstStep (model^.strategy) term defs)) Nothing defs

next :: Model -> Model
next model = model & (output . graph . _Just . _2) %~ (newGraph :)
  where
    newGraph =
      nextGraph
        (model ^. strategy)
        (model ^?! (output . graph . _Just . _1))
        (model ^?! (output . graph . _Just . _2 . _head . _2))
        (model ^. (output . definitions))

prev :: Model -> Model
prev = (output . graph . _Just . _2) %~ drop 1

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval model = noEff $ eval model
updateModel (TermInput newInput) model = noEff $ model & termInput .~ turnBackslashIntoLambda newInput
updateModel (DefInput newInput) model = noEff $ model & defInput .~ turnBackslashIntoLambda newInput
updateModel CBNeed model = noEff $ eval $ model & strategy .~ CallByNeed
updateModel CBName model = noEff $ eval $ model & strategy .~ CallByName
updateModel CBValue model = noEff $ eval $ model & strategy .~ CallByValue
updateModel Clear model = noEff $ model & termInput .~ "" & output .~ Output Nothing Nothing []
updateModel Next model = noEff $ next model
updateModel Prev model = noEff $ prev model
updateModel NoOp model = noEff model