module Main where

import Prelude

import Affjax (Request)
import Affjax as Affjax
import Affjax.ResponseFormat as AffResp
import Audio.Euterpea.ToMelody (perf2melody)
import Audio.SoundFont (Instrument, loadInstruments, loadRemoteSoundFonts)
import Audio.SoundFont.Melody (PMelody(..))
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Euterpea.Midi.MEvent (perform)
import Data.Euterpea.Music (Music, Pitch, (:+:), (:=:))
import Data.Euterpea.Notes (a, b, bn, c, d, e, f, g, wn)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Midi.Instrument (InstrumentName(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import PlayerComponent as PlayerComponent
import React.Basic.DOM (render)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Scientist = Int

newtype State = State {scientists :: Array Scientist}

data Action = UpdateScientists (Array Scientist)

reducer :: State -> Action -> State
reducer (State state) (UpdateScientists scientists) = State (state {scientists = scientists})

options :: Request Json
options = Affjax.defaultRequest { method = Left GET , responseFormat = AffResp.json}

type ErrorMessage = String

initialState :: State
initialState = State {scientists : []}

twoFiveOneinC :: Music Pitch
twoFiveOneinC =
  let 
    dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
    gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
    cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
  in dMinor :+: gMajor :+: cMajor

loadInst :: Aff (Array Instrument)
loadInst = do
  inst <- loadRemoteSoundFonts [ AltoSax ]
  pure inst

component1 :: Array Instrument -> Component Unit
component1 instruments2 = do
    let 
      performance = perform twoFiveOneinC
      instruments = Map.fromFoldable [(AltoSax /\ 0)]
      mel = perf2melody instruments performance
 
    player <- PlayerComponent.component (PMelody mel) instruments2  
    React.component "test comp" (\props -> React.do
        State state /\ dispatch <- React.useReducer initialState reducer
        
        pure $ (DOM.div {children : [player unit]})
    )
main :: Effect Unit
main = launchAff_ do
  container <- liftEffect $ getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  instruments <- (loadInst)
  liftEffect $ case container of
    Nothing -> throw "Container element not found."
    Just c -> do
      ex <- (component1 instruments)
      render (ex unit) c

  