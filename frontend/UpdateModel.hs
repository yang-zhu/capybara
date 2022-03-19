module UpdateModel(updateModel) where

import Control.Lens ((&), (^.), (%~), (.~), (^?!), _1, _2, _Just, _head)
import Miso ((<#), Arrows(Arrows), Effect, noEff, focus, blur, KeyCode (KeyCode))
import Miso.String (MisoString, fromMisoString, toMisoString)

import Lexer
import Parser
import GraphReduction
import Model


-- | Turns the backslashes in the input into λ symbols
turnBackslashIntoLambda :: MisoString -> MisoString
turnBackslashIntoLambda = toMisoString . map (\c -> if c =='\\' then 'λ' else c) . fromMisoString

-- | Updates the model when the Eval action is triggered.
-- Parses the input term and definitions, generates the first two graphs.
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

-- | Updates the model when the Next action is triggered.
-- Compute the next graph and cons it to the list of already computed graphs.
-- To handle divergent terms, a new graph is only computed when the Next action is triggered.
next :: Model -> Model
next model = model & (output . graph . _Just) %~ (newGraph :)
  where
    newGraph =
      nextGraph
        (model ^. strategy)
        (model ^?! (output . graph . _Just . _head . _2))
        (model ^. (output . definitions))

-- | Updates the model when the Prev action is triggered.
-- Removes the most recent graph from the list of already computed graphs.
prev :: Model -> Model
prev = (output . graph . _Just) %~ drop 1

updateModel :: Action -> Model -> Effect Action Model
updateModel Eval model = eval model <# (blur "term-input" >> return NoOp)
updateModel (TermInput newInput) model = noEff $ model & termInput .~ turnBackslashIntoLambda newInput
updateModel (DefInput newInput) model = noEff $ model & defInput .~ turnBackslashIntoLambda newInput
updateModel CBNeed model = noEff $ eval $ model & strategy .~ CallByNeed
updateModel CBName model = noEff $ eval $ model & strategy .~ CallByName
updateModel CBValue model = noEff $ eval $ model & strategy .~ CallByValue
updateModel Clear model = (model & termInput .~ "" & output .~ Output Nothing Nothing []) <# (focus "term-input" >> return NoOp)
updateModel Next model = noEff $ next model
updateModel Prev model = noEff $ prev model
updateModel NoOp model = noEff model