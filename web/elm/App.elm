module App where

import Html

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Time
import Window
import Random
import Text
import Array

-- MODEL
type alias WebEvent =
  { text: String
  , eventType: EventType
  , position: (Int, Int)
  , createdAt: Time.Time
  , updatedAt: Time.Time
  , count: Int
  }

type alias Model =
  List WebEvent

type EventType = Positive | Negative | Neutral

init : String -> EventType -> (Int, Int) -> Time.Time -> Time.Time -> Int -> WebEvent
init text eventType position createdAt updatedAt count =
  { text = text
  , eventType = eventType
  , position = position
  , createdAt = createdAt
  , updatedAt = updatedAt
  , count = count
  }

randomPoint : Int -> Int -> Random.Generator (Int, Int)
randomPoint windowX windowY =
  let
    minX = (-windowX // 2)
    maxX = (windowX // 2)
    minY = (-windowY // 2)
    maxY = (windowX // 2)
  in
    Random.pair (Random.int minX maxX) (Random.int minY maxY)

initialModel : Model
initialModel =
  []

-- UPDATE

type Action = NoOp | Tick Time.Time | SlowTick Time.Time (Int, Int)

eventTypeToColor : EventType -> Color
eventTypeToColor eventType =
  case eventType of
    Positive ->
      green
    Negative ->
      red
    Neutral ->
      gray

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Tick time ->
      let
        isFadedOut = (\webEvent -> (time - webEvent.createdAt) < fadeOutTime )
        tickWebEvent = (\webEvent -> { webEvent | count = webEvent.count + 1, updatedAt = time })
      in
      List.filter isFadedOut model
      |> List.map tickWebEvent 
    SlowTick time (x, y) ->
      let
        maybeEventType = Array.get ((round time) % 3) (Array.fromList [Positive, Negative, Neutral])
        position = Random.generate (randomPoint x y) (Random.initialSeed (round time)) |> fst
        eventType = case maybeEventType of
          Just t ->
            t
          _ ->
            Neutral

        newWebEvent = init "This is some text" eventType position time time 0
      in
         newWebEvent::model

-- VIEW

drawCircle radius color alphaValue (positionX, positionY) =
  circle radius
  |> (filled color) -- Must correspond to type
  |> move ((toFloat positionX), (toFloat positionY))
  |> (alpha alphaValue) -- Must degrade over time

fade : Time.Time -> Time.Time -> Time.Time -> Float
fade fadeOutIn currentT createdAt =
  let
    timePassed = createdAt - currentT
    rawFade =  (timePassed + fadeOutIn) / fadeOutIn
  in
    if rawFade > 0 then rawFade else 0

toFloatPosition : (Int, Int) -> (Float, Float)
toFloatPosition (x, y) =
  (toFloat x, toFloat y)

textString : String -> (Int, Int) -> Float -> Form
textString text (positionX, positionY) fade = Text.fromString(if fade > 0 then text else "")
  |> centered
  |> size 200 200
  |> opacity fade
  |> toForm
  |> move ((toFloat positionX), (toFloat(positionY - 80)))

zip = List.map2 (,)

rippleTime = 2000
fadeOutTime = 20000

view : (Int, Int) -> Model -> Time.Time -> Element
view (w, h) webEvents currentT =
  let
    rippleFade = fade (rippleTime) currentT
    colorFade = fade (fadeOutTime) currentT

    circles = List.map (\(webEvent) ->
      [ drawCircle 140 gray (rippleFade webEvent.createdAt) webEvent.position
      , drawCircle 100 (eventTypeToColor webEvent.eventType) (colorFade webEvent.createdAt) webEvent.position
      , textString webEvent.text webEvent.position (colorFade webEvent.createdAt)
      ]) webEvents
      |> List.concatMap identity
  in
    above (collage w h circles) (show (currentT,  webEvents, (List.length webEvents)))

-- SIGNAL
delta : Signal Time.Time
delta = (Time.fps 10)

timeStream : Signal (Time.Time, Time.Time)
timeStream = (Time.timestamp delta)

ticker : Signal Action
ticker =
  Signal.map (\(currentTime, _) -> Tick currentTime) timeStream

slowTicker : Signal Action
slowTicker =
  let
    slowTicksCurrentTime = (Signal.map SlowTick currentTime)
    slowTicksCurrentTimeAndWindow = Signal.map2 (\slowTick dimensions -> slowTick dimensions) slowTicksCurrentTime Window.dimensions
  in
    Signal.sampleOn (Time.fps 0.5) slowTicksCurrentTimeAndWindow

input : Signal Action
input =
  Signal.mergeMany [ticker, slowTicker]

model : Signal Model
model =
  Signal.foldp update initialModel input

initialSeed : Float -> Random.Seed
initialSeed time = Random.initialSeed (round time)

currentTime : Signal Time.Time
currentTime =
  Signal.map fst timeStream

-- MAIN

main : Signal Element
main =
  Signal.map3 view Window.dimensions model currentTime
