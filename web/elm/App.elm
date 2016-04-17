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
import Task exposing (Task)
import Effects exposing (Effects)

-- CONFIG

rippleTime = 2000
fadeOutTime = 20000

-- MISC

initialSeed : Float -> Random.Seed
initialSeed time = Random.initialSeed (round time)

randomPoint : Int -> Int -> Random.Generator (Int, Int)
randomPoint windowX windowY =
  let
    minX = (-windowX // 2)
    maxX = (windowX // 2)
    minY = (-windowY // 2)
    maxY = (windowX // 2)
  in
    Random.pair (Random.int minX maxX) (Random.int minY maxY)

toFloatPosition : (Int, Int) -> (Float, Float)
toFloatPosition (x, y) =
  (toFloat x, toFloat y)

zip = List.map2 (,)

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

eventTypeToColor : EventType -> Color
eventTypeToColor eventType =
  case eventType of
    Positive ->
      green
    Negative ->
      red
    Neutral ->
      gray

init : String -> EventType -> (Int, Int) -> Time.Time -> Time.Time -> Int -> WebEvent
init text eventType position createdAt updatedAt count =
  { text = text
  , eventType = eventType
  , position = position
  , createdAt = createdAt
  , updatedAt = updatedAt
  , count = count
  }

initialModel : Model
initialModel =
  []

type alias RawWebEvent =
  { text: String
  , eventType: String
  }

-- UPDATE

type Action = NoOp | Tick | SlowTick (Int, Int) | AddWebEvent RawWebEvent (Int, Int) | Increment

update : (Time.Time, Action) -> Model -> Model
update (timeNow, action) model =
  case action of
    NoOp ->
      model
    Increment ->
      List.map (\webEvent -> { webEvent | count = webEvent.count + 100}) model
    Tick ->
      let
        isFadedOut = (\webEvent -> (timeNow - webEvent.createdAt) < fadeOutTime )
        tickWebEvent = (\webEvent -> { webEvent | updatedAt = timeNow })
      in
        -- List.filter isFadedOut model |> List.map tickWebEvent
        model
    SlowTick (x, y) ->
      let
        position = Random.generate (randomPoint x y) (initialSeed timeNow) |> fst
        maybeEventType = Array.get ((round timeNow) % 3) (Array.fromList [Positive, Negative, Neutral])
        eventType = case maybeEventType of
          Just t ->
            t
          _ ->
            Neutral

        newWebEvent = init "This is some text" eventType position timeNow timeNow 0
      in
         newWebEvent::model
         --model
    AddWebEvent rawWebEvent (x, y) ->
      let
        position = Random.generate (randomPoint x y) (initialSeed timeNow) |> fst
        eventType = case rawWebEvent.eventType of
          "positive" ->
            Positive
          "negative" ->
            Negative
          "neutral" ->
            Neutral
          _ ->
            Neutral

        newWebEvent = init rawWebEvent.text eventType position timeNow timeNow 0
      in
         newWebEvent::model

-- VIEW

drawCircle radius color alphaValue (positionX, positionY) =
  circle radius
  |> (filled color)
  |> move ((toFloat positionX), (toFloat positionY))
  |> (alpha alphaValue)

fade : Time.Time -> Time.Time -> Time.Time -> Float
fade fadeOutIn currentT createdAt =
  let
    timePassed = createdAt - currentT
    rawFade =  (timePassed + fadeOutIn) / fadeOutIn
  in
    if rawFade > 0 then rawFade else 0

textString : String -> (Int, Int) -> Float -> Form
textString text (positionX, positionY) fade = Text.fromString(if fade > 0 then text else "")
  |> centered
  |> size 200 200
  |> opacity fade
  |> toForm
  |> move ((toFloat positionX), (toFloat(positionY - 80)))

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
--    above (collage w h circles) (show (currentT,  webEvents, (List.length webEvents), List.map (\w -> currentT - w.createdAt) webEvents))
    (collage w h circles)

-- SIGNAL

currentTime : Signal Time.Time
currentTime =
  Signal.map fst timeStream

delta : Signal Time.Time
delta = (Time.fps 20)

input : Signal Action
input =
  Signal.mergeMany [ticker, slowTicker, mappedJsActions]--, (Signal.map3 AddWebEvent webEvents currentTime Window.dimensions)]

model : Signal Model
model =
  Signal.foldp update initialModel (Time.timestamp input)

slowTicker : Signal Action
slowTicker =
  Signal.map SlowTick  Window.dimensions
    |> Signal.sampleOn (Time.fps 0.5)

ticker : Signal Action
ticker =
  Signal.map (always Tick) timeStream

timeStream : Signal (Time.Time, Time.Time)
timeStream = (Time.timestamp delta)

-- PORTS

--port webEvents : Signal RawWebEvent

--port sound : Signal String
--port sound =
--  inbox.signal

--inbox : Signal.Mailbox String
--inbox =
--  Signal.mailbox "none"

--port runner : Task x ()
--port runner =
--  Signal.send inbox.address "sup yo?"

port jsActions : Signal RawWebEvent

mapJsActions : RawWebEvent -> (Int, Int) -> Action
mapJsActions rawWebEvent position =
  AddWebEvent rawWebEvent position

mappedJsActions : Signal Action
mappedJsActions =
  Signal.map2 mapJsActions jsActions Window.dimensions

-- MAIN

main : Signal Element
main =
  Signal.map3 view Window.dimensions model currentTime
