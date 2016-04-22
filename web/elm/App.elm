module App (..) where

import AnimationFrame
import Array
import Color exposing (..)
import Effects exposing (Effects)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html
import Random
import StartApp
import Task exposing (Task)
import Text
import Time
import VirtualDom exposing (Node)
import Window


-- CONFIG


fadeOutTime =
  20000


rippleTime =
  2000


soundNames =
  [ "another-one.mp3"
  , "beatking_holdup.mp3"
  , "claves.mp3"
  , "conga2.mp3"
  , "conga4.mp3"
  , "cowbell.mp3"
  , "crash2.mp3"
  , "drake-ugh.mp3"
  , "drake-yeauh.mp3"
  , "guitar-palm-1.mp3"
  , "guitar-palm-2.mp3"
  , "guitar-palm-3.mp3"
  , "guitar-palm-4.mp3"
  , "guitar-palm-5.mp3"
  , "gunshot3.mp3"
  , "hat-1.mp3"
  , "hat.mp3"
  , "hay.mp3"
  , "jersey-ha.mp3"
  , "jyea.mp3"
  , "keys1.mp3"
  , "keys2.mp3"
  , "keys3.mp3"
  , "keys4.mp3"
  , "keys5.mp3"
  , "keys6.mp3"
  , "metronome-up.mp3"
  , "metronome.mp3"
  , "normal3.mp3"
  , "punchy-trap.mp3"
  , "ross-huh.mp3"
  , "ross-maybach.mp3"
  , "smooth5.mp3"
  , "snap.mp3"
  , "snare-huge-reverb.mp3"
  , "snare-rim-real.mp3"
  , "stick.mp3"
  , "tom.mp3"
  , "tom2.mp3"
  , "trap-snare-dry.mp3"
  , "triangle.mp3"
  , "ugh.mp3"
  , "yeahbaby.mp3"
  ]



-- MISC


soundName : Sound -> String
soundName sound =
  case sound of
    Beep ->
      "keys4.mp3"

    Bop ->
      "keys5.mp3"

    Boop ->
      "keys6.mp3"

    None ->
      ""


initialSeed : Float -> Random.Seed
initialSeed time =
  Random.initialSeed (round time)


randomPoint : ( Int, Int ) -> Random.Generator ( Int, Int )
randomPoint ( windowX, windowY ) =
  let
    minX =
      (-windowX // 2)

    maxX =
      (windowX // 2)

    minY =
      (-windowY // 2)

    maxY =
      (windowX // 2)
  in
    Random.pair (Random.int minX maxX) (Random.int minY maxY)



-- MODEL


type alias WebEvent =
  { text : String
  , eventType : EventType
  , position : ( Int, Int )
  , createdAt : Time.Time
  , updatedAt : Time.Time
  , count : Int
  }


type alias Model =
  { webEvents : List WebEvent
  , time : Time.Time
  , dimensions : ( Int, Int )
  }


type Sound
  = Beep
  | Bop
  | Boop
  | None


type EventType
  = Positive
  | Negative
  | Neutral


eventTypeToColor : EventType -> Color
eventTypeToColor eventType =
  case eventType of
    Positive ->
      green

    Negative ->
      red

    Neutral ->
      white


eventTypeToSound : EventType -> Sound
eventTypeToSound eventType =
  case eventType of
    Positive ->
      Beep

    Negative ->
      Bop

    Neutral ->
      None


initialModel : Model
initialModel =
  { webEvents = []
  , time = 0
  , dimensions = ( 0, 0 )
  }


type alias RawWebEvent =
  { text : String
  , eventType : String
  }



-- UPDATE


type Action
  = NoOp
  | Tick Time.Time ( Int, Int )
  | AddWebEvent Time.Time RawWebEvent ( Int, Int )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Tick timeNow dimensions ->
      let
        isFadedOut =
          (\webEvent -> (timeNow - webEvent.createdAt) < fadeOutTime)

        tickWebEvent =
          (\webEvent -> { webEvent | updatedAt = timeNow })

        newWebEvents =
          List.filter isFadedOut model.webEvents
            |> List.map tickWebEvent

        newModel =
          { model | webEvents = newWebEvents, time = timeNow, dimensions = dimensions }
      in
        ( newModel, Effects.none )

    AddWebEvent timeNow rawWebEvent dimensions ->
      let
        position =
          Random.generate (randomPoint dimensions) (initialSeed timeNow) |> fst

        maybeEventType =
          case rawWebEvent.eventType of
            "positive" ->
              Just Positive

            "negative" ->
              Just Negative

            _ ->
              Nothing

        returnTupple =
          case maybeEventType of
            Just eventType ->
              ( { model
                  | webEvents = ((WebEvent rawWebEvent.text eventType position timeNow timeNow 0) :: model.webEvents)
                  , time = timeNow
                  , dimensions = dimensions
                }
              , (sendSound (eventTypeToSound eventType))
              )

            Nothing ->
              ( model, Effects.none )
      in
        returnTupple



-- VIEW


drawCircle : Float -> Color -> Float -> ( Int, Int ) -> Form
drawCircle radius color alphaValue ( positionX, positionY ) =
  circle radius
    |> (filled color)
    |> move ( (toFloat positionX), (toFloat positionY) )
    |> (alpha alphaValue)


fade : Time.Time -> Time.Time -> Time.Time -> Float
fade fadeOutIn currentT createdAt =
  let
    timePassed =
      createdAt - currentT

    rawFade =
      (timePassed + fadeOutIn) / fadeOutIn
  in
    if rawFade > 0 then
      rawFade
    else
      0


textString : String -> ( Int, Int ) -> Float -> Form
textString text ( positionX, positionY ) fade =
  Text.fromString
    (if fade > 0 then
      text
     else
      ""
    )
    |> centered
    |> size 200 200
    |> opacity fade
    |> toForm
    |> move ( (toFloat positionX), (toFloat (positionY - 80)) )


view : Signal.Address Action -> Model -> VirtualDom.Node
view address model =
  let
    rippleFade =
      fade (rippleTime) model.time

    colorFade =
      fade (fadeOutTime) model.time

    width =
      fst model.dimensions

    height =
      snd model.dimensions

    circles =
      List.map
        (\webEvent ->
          [ drawCircle 140 gray (rippleFade webEvent.createdAt) webEvent.position
          , drawCircle 100 (eventTypeToColor webEvent.eventType) (colorFade webEvent.createdAt) webEvent.position
          , textString webEvent.text webEvent.position (colorFade webEvent.createdAt)
          ]
        )
        model.webEvents
        |> List.concatMap identity
  in
    collage width height circles |> Html.fromElement


currentTime : Signal Time.Time
currentTime =
  Signal.map fst timeStream


inputs : List (Signal Action)
inputs =
  [ ticker, mappedRawWebEvents ]


ticker : Signal Action
ticker =
  Signal.map2 Tick currentTime Window.dimensions


timeStream : Signal ( Time.Time, Time.Time )
timeStream =
  (Time.timestamp AnimationFrame.frame)



-- PORTS


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


sendSound : Sound -> Effects Action
sendSound sound =
  Signal.send soundMailbox.address (sound |> soundName)
    |> Effects.task
    |> Effects.map (always NoOp)


port sounds : Signal String
port sounds =
  soundMailbox.signal


soundMailbox : Signal.Mailbox String
soundMailbox =
  Signal.mailbox ""


port rawWebEvent : Signal RawWebEvent
mapRawWebEvents : Time.Time -> RawWebEvent -> ( Int, Int ) -> Action
mapRawWebEvents time rawWebEvent position =
  AddWebEvent time rawWebEvent position


mappedRawWebEvents : Signal Action
mappedRawWebEvents =
  Signal.map3 mapRawWebEvents currentTime rawWebEvent Window.dimensions



-- MAIN


app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = inputs
    }


main : Signal Node
main =
  app.html
