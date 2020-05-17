module PlayerComponent where

-- | A Halogen component which acts as a generic player for any music
-- | source that can be converted into a Melody - in other words
-- | one that is an instance of Playable

-- | When looking at the data structures, whenever we have a type variable p
-- | then look at it as if it were defined as ∀ p. Playable p => p
-- | It doesn't seem possible actually to define a type like this and so the
-- | Query and State data types have been parameterised with (simply) p
-- | but all functions that involve these types require the Playable constraint.

import Prelude

import Audio.SoundFont (Instrument, playNotes, instrumentChannels)
import Audio.SoundFont.Melody (Melody, MidiPhrase)
import Audio.SoundFont.Melody.Class (class Playable, toMelody)
import Data.Array (null, index, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React



data PlayBackState =
    PLAYING
  | PENDINGPAUSED
  | PAUSED

type Dispatch p = Action p -> Effect Unit

derive instance genericPlaybackState :: Generic PlayBackState _
instance showEvent :: Show PlayBackState where
  show = genericShow
instance eqEvent :: Eq PlayBackState where
  eq = genericEq

data Messge = IsPlaying Boolean

data Action p = 
    SetInstruments (Array Instrument)
  | PlayMelody PlayBackState
  | StepMelody
  | StopMelody
  | EnablePlayButton
  | HandleNewPlayable p


data Action2 p = SetState (State p)

newtype State p = State 
    { instruments :: Array Instrument
    , melody :: Melody
    , playing :: PlayBackState
    , phraseIndex :: Int
    , phraseLength :: Number
    , playable :: p
    , nextAction :: Maybe (Action p)
    , playNum :: Number
    }

reducer :: forall p. Playable p => State p -> Action2 p -> State p
reducer state (SetState (State newState)) = State (newState)

runAction :: forall p. Playable p => State p -> Action p -> Aff (Tuple (State p) (Maybe (Action p)))
runAction (State state) (SetInstruments instruments) = do
    let
        newPlayingState = 
            if (state.playing == PLAYING) then PENDINGPAUSED
            else PAUSED
    pure (State (state {instruments = instruments, playing = newPlayingState}) /\ Nothing)
runAction (State state) (PlayMelody playBack) = do
    let 
        (State newState) = 
            if (null state.melody) then
                establishMelody (State state) 
            else 
                (State state)
    if ((playBack == PLAYING) && (not (null state.melody))) then do
        pure ((State (newState {playing = PLAYING})) /\ (Just StepMelody))
    else do
        let pendingPaused = newState {playing = PENDINGPAUSED}
        nextInstr <- temporarilyFreezePlayButton (State state)
        pure ((State newState) /\ (Just nextInstr))
runAction (State state) (StepMelody) = do
    if ((state.playing == PLAYING) && not (null state.melody)) then do
        step (State state)
    else do
        let newState = state {playing = PENDINGPAUSED}
        nextInstr <- temporarilyFreezePlayButton (State state)
        pure ((State newState) /\ (Just nextInstr))  
runAction (State state) (StopMelody) = do
    stop (State state)
runAction (State state) (EnablePlayButton) = do
    let newState = state {playing = PAUSED}
    pure ((State newState) /\ Nothing)
runAction (State state) (HandleNewPlayable playable') = do
    (State newState) /\ nextAction <- stop (State state)
    let stateWithNewPlayable = newState {playable = playable', melody = []}
    pure ((State stateWithNewPlayable) /\ Nothing)
    

playAudio :: forall p. Playable p => ((Action2 p) -> Effect Unit) -> State p -> Maybe (Action p) -> Effect Unit
playAudio dispatch (State state) nextAction = launchAff_ do
    case nextAction of
        Just action -> do
            (State nextState) /\ newAction <- runAction (State state) action
            let stateWithAction = State (nextState {nextAction = newAction, playNum = nextState.playNum + 1.0})
            liftEffect $ dispatch (SetState stateWithAction)
        _ -> pure unit

initialState :: forall p. Playable p => Array Instrument -> p -> State p
initialState instruments playable = 
    State 
        { instruments : instruments
        , melody : []
        , playing : PAUSED
        , phraseIndex : 0
        , phraseLength : 0.0
        , playable : playable
        , nextAction : Nothing
        , playNum : 0.0
        }

establishMelody :: forall p. Playable p => State p -> (State p)
establishMelody (State state)  =
    let melody = toMelody state.playable (instrumentChannels state.instruments) in
    State (state {melody = melody})

stop :: forall p. Playable p => State p -> Aff (Tuple (State p) (Maybe (Action p)))
stop (State state)  =
    if (state.playing == PLAYING) then do
        nextInstr <- temporarilyFreezePlayButton (State state)
        let newState = state {playing = PAUSED}
        pure (State newState /\ Nothing)
    else do 
        let newState = state {playing = PAUSED}
        pure (State newState /\ Nothing)

step :: forall p. Playable p => State p ->  Aff (Tuple (State p) (Maybe (Action p)))
step (State state) = do
    let mPhrase = locateNextPhrase (State state)
    case mPhrase of
        Just (midiPhrase) -> do
            phraseLength <- liftEffect (playEvent state.instruments midiPhrase)
            let newState = state {phraseIndex = state.phraseIndex + 1, phraseLength = phraseLength}
            _ <- delay (Milliseconds (phraseLength * 1000.0))
            pure (State newState /\ Just StepMelody)
        _ -> pure (State state /\ Just StopMelody)
        

temporarilyFreezePlayButton :: forall p. Playable p => State p -> Aff (Action p)
temporarilyFreezePlayButton (State state) = do
    let msDelay = state.phraseLength
    _ <- delay (Milliseconds (msDelay * 1000.0))
    pure (EnablePlayButton)
  

locateNextPhrase :: forall p. Playable p => State p -> Maybe MidiPhrase
locateNextPhrase (State state) =
    if (not (state.playing == PLAYING)) || (null state.melody) then
        Nothing
    else 
        index state.melody (state.phraseIndex)

playEvent :: Array Instrument -> MidiPhrase -> Effect Number
playEvent instruments midiPhrase =
    playNotes instruments midiPhrase


component :: forall p. Playable p => p -> Array Instrument -> Component Unit
component playable instruments =
    React.component "Soundfont Player" (\props -> React.do
        State state /\ dispatch <- React.useReducer (initialState instruments playable) reducer
        let 
            sliderPos = toNumber $ state.phraseIndex
            capsuleMax = toNumber $ length state.melody  
            startImg = "assets/images/play.png"
            stopImg = "assets/images/stop.png"
            pauseImg = "assets/images/pause.png"
            playAction =
                if (state.playing == PLAYING) then
                    PlayMelody PLAYING
                else 
                    PlayMelody PLAYING
            playButtonImg =
                if (state.playing == PLAYING) then 
                    pauseImg
                else
                    startImg 
            isDisabled = (state.playing == PENDINGPAUSED)
        
        React.useEffect state.playNum $ do
            playAudio dispatch (State state) (state.nextAction)
            pure (pure unit) 
        pure $ DOM.div {
            children : 
                [ DOM.div {
                    children : 
                        [ DOM.input 
                            { type : "image"
                            , disabled : isDisabled
                            , src : playButtonImg
                            , onClick :handler_ (playAudio dispatch (State state) (Just playAction))
                            }
                        , DOM.input 
                            { type: "image"
                            , disabled : isDisabled
                            , src : stopImg
                            , onClick : handler_ (playAudio dispatch (State state) (Just StopMelody))
                            }
                        , DOM.progress 
                            { max : capsuleMax
                            , value : show sliderPos
                            }
                        ]
                 }
                ]
        }
    )