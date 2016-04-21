module App (..) where

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
import StartApp
import Effects exposing (Effects)
import VirtualDom exposing (Node)


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


toFloatPosition : ( Int, Int ) -> ( Float, Float )
toFloatPosition ( x, y ) =
  ( toFloat x, toFloat y )



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


init : String -> EventType -> ( Int, Int ) -> Time.Time -> Time.Time -> Int -> WebEvent
init =
  WebEvent


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
  | Increment


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Increment ->
      let
        newWebEvents =
          List.map (\webEvent -> { webEvent | count = webEvent.count + 100 }) model.webEvents
      in
        ( { model | webEvents = newWebEvents }, Effects.none )

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
          case maybeEventType of
            Just t ->
              t

            _ ->
              Neutral

        newWebEvent =
          init "This is some text" eventType position timeNow timeNow 0

        mewmodel =
          { model | webEvents = newWebEvent :: model.webEvents, dimensions = ( x, y ), time = timeNow }
      in
        ( mewmodel, sendSound "some sound" )

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
          init rawWebEvent.text eventType position timeNow timeNow 0

        mewmodel =
          { model | webEvents = newWebEvent :: model.webEvents, time = timeNow }
      in
        ( mewmodel, sendSound "some sound" )



-- VIEW


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
    --    above (collage width height circles) (show ( model.time, model, (List.length model.webEvents) ))
    --above (collage width height circles) (show ( model.time, model, (List.length model.webEvents) )) |> Html.fromElement
    collage width height circles |> Html.fromElement



currentTime : Signal Time.Time
currentTime =
  Signal.map fst timeStream


delta : Signal Time.Time
delta =
  (Time.fps 10)


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
  (Time.timestamp delta)



-- PORTS


port tasks : Signal String
port tasks =
  outgoing.signal


sendSound : String -> Effects Action
sendSound sound =
  Signal.send outgoing.address sound
    |> Effects.task
    |> Effects.map (always NoOp)


outgointSignal : Signal String
outgointSignal =
  outgoing.signal


outgoing : Signal.Mailbox String
outgoing =
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
