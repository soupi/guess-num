module Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Control.MonadPlus (guard)
import Data.Array (zipWith, length, reverse)
import Data.Foldable (sum)
import Data.Int (toNumber, floor)
import Data.List (head, (:), List(Nil))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (toCharArray, trim)
import Data.Traversable (traverse)
import Halogen (ComponentHTML, ComponentDSL, Component, HalogenEffects, set, fromEff, modify, component, runUI)
import Halogen.HTML (a, text, button, p_, input, span_, h1_, div_)
import Halogen.HTML.Events (input, input_, onClick) as E
import Halogen.HTML.Events.Forms (onValueInput)
import Halogen.HTML.Properties (href, value)
import Halogen.Util (awaitBody, runHalogenAff)
import Math (pow)
import Prelude hiding (Ordering(..))

main :: Eff (AppEffects ()) Unit              
main = runHalogenAff do
  body <- awaitBody
  state <- fromEff initialState
  runUI ui state body

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

type AppEffects eff = HalogenEffects (random :: RANDOM | eff)

ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component { render, eval }

eval :: forall eff. Query ~> ComponentDSL State Query (Aff (AppEffects eff))
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
    newState <- fromEff initialState
    set newState
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
inputToGuess inp = do
  digArr <- map reverse <<< traverse toDigit <<< toCharArray <<< trim $ inp
  guard (0 < length digArr && length digArr < 3)
  pure $ sum $ zipWith (\n p -> floor (toNumber n * pow 10.0 p)) digArr [0.0,1.0]

toDigit :: Char -> Maybe Int
toDigit = case _ of
  '0' -> pure 0
  '1' -> pure 1
  '2' -> pure 2
  '3' -> pure 3
  '4' -> pure 4
  '5' -> pure 5
  '6' -> pure 6
  '7' -> pure 7
  '8' -> pure 8
  '9' -> pure 9
  _   -> Nothing

  

  
