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


rippleTime =
  2000


fadeOutTime =
  20000



-- MISC


initialSeed : Float -> Random.Seed
initialSeed time =
  Random.initialSeed (round time)


randomPoint : Int -> Int -> Random.Generator ( Int, Int )
randomPoint windowX windowY =
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
      gray


eventTypeToSound : EventType -> Sound
eventTypeToSound eventType =
  case eventType of
    Positive ->
      Beep

    Negative ->
      Bop

    Neutral ->
      Boop


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
  | Tick Time.Time
  | SlowTick Time.Time ( Int, Int )
  | AddWebEvent Time.Time RawWebEvent ( Int, Int )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Tick timeNow ->
      let
        isFadedOut =
          (\webEvent -> (timeNow - webEvent.createdAt) < fadeOutTime)

        tickWebEvent =
          (\webEvent -> { webEvent | updatedAt = timeNow })

        newWebEvents =
          List.filter isFadedOut model.webEvents
            |> List.map tickWebEvent

        newModel =
          { model | webEvents = newWebEvents, time = timeNow }
      in
        ( newModel, Effects.none )

    SlowTick timeNow ( x, y ) ->
      let
        position =
          Random.generate (randomPoint x y) (initialSeed timeNow) |> fst

        maybeEventType =
          Array.get ((round timeNow) % 3) (Array.fromList [ Positive, Negative, Neutral ])

        eventType =
          (Maybe.withDefault Neutral maybeEventType)

        newWebEvent =
          WebEvent "This is some text" eventType position timeNow timeNow 0

        mewmodel =
          { model | webEvents = newWebEvent :: model.webEvents, dimensions = ( x, y ), time = timeNow }

        sound =
          eventType |> eventTypeToSound
      in
        ( mewmodel, sendSound sound )

    --model
    AddWebEvent timeNow rawWebEvent ( x, y ) ->
      let
        position =
          Random.generate (randomPoint x y) (initialSeed timeNow) |> fst

        eventType =
          case rawWebEvent.eventType of
            "positive" ->
              Positive

            "negative" ->
              Negative

            "neutral" ->
              Neutral

            _ ->
              Neutral

        newWebEvent =
          WebEvent rawWebEvent.text eventType position timeNow timeNow 0

        sound =
          eventType |> eventTypeToSound

        mewmodel =
          { model | webEvents = newWebEvent :: model.webEvents, time = timeNow }
      in
        ( mewmodel, sendSound sound )



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
  [ ticker, slowTicker, mappedRawWebEvents ]


slowTicker : Signal Action
slowTicker =
  Signal.map2 SlowTick currentTime Window.dimensions
    |> Signal.sampleOn (Time.fps 0.5)


ticker : Signal Action
ticker =
  Signal.map Tick currentTime


timeStream : Signal ( Time.Time, Time.Time )
timeStream =
  (Time.timestamp AnimationFrame.frame)



-- PORTS


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


sendSound : Sound -> Effects Action
sendSound sound =
  Signal.send soundMailbox.address (sound |> toString)
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
