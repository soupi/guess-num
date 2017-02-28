module Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Data.Int (fromString)
import Data.List (head, (:), List(Nil))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (trim)
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftEff, modify, put)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.HTML (HTML, a, text, button, p_, input, span_, h1_, div_)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Events (input, input_, onClick) as E
import Halogen.HTML.Properties (href, value)
import Halogen.VDom.Driver (runUI)
import Prelude hiding (Ordering(..))

main :: Eff (HalogenEffects (random :: RANDOM)) Unit
main = runHalogenAff do
  body <- awaitBody
  state <- liftEff initialState
  runUI (ui state) unit body

type State =
  { target :: Int
  , guess :: String
  , attempts :: List String
  , validAttempts :: Int
  }

initialState :: forall eff. Eff (random :: RANDOM | eff) State
initialState = do
  target <- randomInt 0 100
  pure
    { target: target
    , guess: ""
    , attempts: Nil
    , validAttempts: 0
    }

data Query a
  = Guess a
  | Restart a
  | SetGuess String a

ui :: forall eff. State -> Component HTML Query Unit Void (Aff (random :: RANDOM | eff))
ui state =
  component
    { initialState: const state
    , render: render
    , eval: eval
    , receiver: const Nothing
    }

eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
eval = case _ of
  SetGuess input next -> do
    modify (_ { guess = input })
    pure next

  Guess next -> do
    modify
      (\s -> if isWinner s then s else
        s { attempts = s.guess : s.attempts
          , validAttempts = if isValid (compareGuess s.guess s.target) then s.validAttempts + 1 else s.validAttempts
          }
      )
    pure next

  Restart next -> do
    newState <- liftEff initialState
    put newState
    pure next

render :: State -> ComponentHTML Query
render s =
   div_ $
      [ h1_ [ text "Guess the number!" ]
      , span_
          [ input [ value s.guess, onValueInput (E.input SetGuess) ]
          , button
            [ E.onClick (E.input_ Guess) ]
            [ text "Guess" ]
          ]
      , p_ [ text $ "Attempts: " <> show s.validAttempts ]
      ]
      <> case head s.attempts of
           Nothing -> []
           Just guess -> [p_ [ text $ "Status:   " <> status (compareGuess guess s.target) ]]
      <> [ p_
             [ button
                 [ E.onClick (E.input_ Restart) ]
                 [ text "Restart" ]
             ]
         , p_
           [ a
             [ href "https://github.com/soupi/guess-num" ]
             [ text "View the code on Github" ]
           ]
         ]

data Cmp = LT | EQ | GT | Invalid

isWinner :: State -> Boolean       
isWinner s = case head s.attempts of
  Nothing -> false
  Just guess -> isEq $ compareGuess guess s.target

status :: Cmp -> String
status LT      = "Too low"
status EQ      = "Winner winner chicken dinner!"
status GT      = "Too high"
status Invalid = "Invalid. Guess must be an integer n where 0 <= n <= 99"

isEq :: Cmp -> Boolean
isEq = case _ of
  EQ -> true
  _  -> false

cmp :: Int -> Int -> Cmp
cmp n m
  | n <  m = LT
  | n == m = EQ
  | n >  m = GT
  | otherwise = Invalid

isValid :: Cmp -> Boolean
isValid = case _ of
  Invalid -> false
  _       -> true

compareGuess :: String -> Int -> Cmp
compareGuess input target =
  maybe Invalid (_ `cmp` target) (inputToGuess input)

inputToGuess :: String -> Maybe Int
inputToGuess = fromString <<< trim
