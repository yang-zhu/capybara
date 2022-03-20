module UpdateModel(updateModel) where

import Data.Maybe (isNothing)
import Control.Lens ((&), (^.), (%~), (.~), (^?!), _1, _2, _Just, _head)
import Miso (Effect, (<#), noEff, focus)
import Miso.String (MisoString, fromMisoString, toMisoString)

import Lexer
import Parser ( parseDefinitions, parseExpression )
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
updateModel Eval model =
  ( model 
      & termInput %~ turnBackslashIntoLambda  -- replace all the backslashes into λs in the input box
      & defInput %~ turnBackslashIntoLambda   -- replace all the backslashes into λs in the definitions
      & eval
  ) <# (focus "next-button" >> return NoOp)   -- removes the focus from the input box
updateModel (TermInput newInput) model = noEff $ model & termInput .~ newInput
updateModel (DefInput newInput) model = noEff $ model & defInput .~ newInput
updateModel CBNeed model = model & strategy .~ CallByNeed & updateModel Eval
updateModel CBName model = model & strategy .~ CallByName & updateModel Eval
updateModel CBValue model = model & strategy .~ CallByValue & updateModel Eval
updateModel Clear model =
  ( model 
      & termInput .~ ""
      & output .~ Output Nothing Nothing []
  ) <# (focus "term-input" >> return NoOp)  -- focuses the input box when the clear button is pressed
updateModel Next model = 
  if isNothing $ model ^?! (output . graph . _Just . _head . _1)
    then noEff model
    else next model <# (focus "next-button" >> return NoOp)  -- focuses the forward button when the last graph is reached
updateModel Prev model = 
  if null $ drop 2 $ model ^?! (output . graph . _Just)
    then noEff model
    else prev model <# (focus "prev-button" >> return NoOp)  -- focuses the backward button when there are only two graphs left
updateModel NoOp model = noEff model