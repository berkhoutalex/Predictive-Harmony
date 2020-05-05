module Main where

import Prelude

import Affjax (Request, printError)
import Affjax as Affjax
import Affjax.ResponseFormat as AffResp
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import Types (Scientist)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

newtype State = State {scientists :: Array Scientist}

data Action = UpdateScientists (Array Scientist)

reducer :: State -> Action -> State
reducer (State state) (UpdateScientists scientists) = State (state {scientists = scientists})

options :: Request Json
options = Affjax.defaultRequest { method = Left GET , responseFormat = AffResp.json}

type ErrorMessage = String

request :: forall resp. DecodeJson resp => String -> Aff (Either ErrorMessage resp)
request url = 
    runExceptT $ do
        {body} <- ExceptT $ Affjax.request (options {url = url}) <#> lmap printError
        except $ decodeJson body

getScientists :: (Action -> Effect Unit) -> Effect Unit
getScientists dispatch = launchAff_ do
  let url = "http://localhost:8001/scientists"
  response <- request url
  case response of
    Right responseData -> liftEffect $ dispatch (UpdateScientists responseData)
    _ -> Console.log "aw darn"

initialState :: State
initialState = State {scientists : []}

component1 :: Component Unit
component1 = do
    React.component "test comp" (\props -> React.do
        State state /\ dispatch <- React.useReducer initialState reducer
        React.useEffect props $ do 
            getScientists dispatch
            pure (pure unit)

        pure $ (DOM.div {children : [DOM.text "got some scientists boi"]})
    )

main :: Effect Unit
main = do
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c -> do
      ex <- component1
      render (ex unit) c

  