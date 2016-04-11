module App where

import Html

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Time
import Window
import Random
import Text

-- MODEL
type alias WebEvent =
  { text: String
  , color: Color          -- This is determined by the type
  , position: (Int, Int)  -- These need to be created randomly within the window dimensions
  --, createdAt: Time.Time       -- This is used to render the ripple effect and determine when the event dissapears
  , count: Int
  }

type alias Model =
  List WebEvent

randomPoint : (Int, Int) -> Random.Generator (Int, Int)
randomPoint (windowX, windowY) =
  let
    minX = (-windowX // 2)
    maxX = (windowX // 2)
    minY = (-windowY // 2)
    maxY = (windowX // 2)
  in
    Random.pair (Random.int minX maxX) (Random.int minY maxY)

initialModel : Model
initialModel =
  [ { text = "hey there hey there hey there hey there hey there hey there hey there hey there hey "
    , color = red
    , position = ( -200, 300)
    , count = 0
    }
  , { text = "User nick@sanford.com signed up as an employer."
    , color = green
    , position = ( 200, -300)
    , count = 0
    }
  ]


-- UPDATE

type Action = NoOp | Tick

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Tick ->
      List.map (\webEvent -> { webEvent | count = webEvent.count + 1 }) model

-- VIEW

drawCircle radius color alphaValue (positionX, positionY) =
  circle radius
  |> (filled color) -- Must correspond to type
  |> move ((toFloat positionX), (toFloat positionY))
  |> (alpha alphaValue) -- Must degrade over time

fade : Time.Time -> Time.Time -> Time.Time -> Float
fade fadeOutIn startT currentT =
  let
    timePassed = startT - currentT
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

view : (Int, Int) -> Time.Time -> Model -> Time.Time -> Element
view (w, h) startT webEvents currentT =
  let
    rippleFade = fade 2000 startT currentT
    colorFade = fade 20000 startT currentT

    circles = List.map (\webEvent ->
      [ drawCircle 140 gray rippleFade webEvent.position
      , drawCircle 100 webEvent.color colorFade webEvent.position
      , textString webEvent.text webEvent.position colorFade
      ]) webEvents
      |> List.concatMap identity
  in
    --above (collage w h circles) (show (List.map (\x -> (x.count, startT, cTime,  Time.inSeconds(cTime - startT), Time.inMilliseconds(cTime - startT))) webEvents))
    above (collage w h circles) (show (startT, currentT,  Time.inSeconds(currentT - startT), Time.inMilliseconds(currentT - startT), rippleFade, colorFade))

-- SIGNAL

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.fps 60)

input : Signal Action
input =
  Signal.mergeMany [ticker]

model : Signal Model
model =
  Signal.foldp update initialModel input

startTime : Signal Time.Time
startTime =
  Signal.map fst (Signal.constant () |> Time.timestamp)

currentTime : Signal Time.Time
currentTime =
  Signal.map fst (Time.timestamp ticker)

-- MAIN

main : Signal Element
main =
  Signal.map4 view Window.dimensions startTime model currentTime
