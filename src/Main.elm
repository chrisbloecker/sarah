module Main exposing (main)
--------------------------------------------------------------------------------
import Html                    exposing (..)
import Html.App        as App
import Html.Attributes         exposing (..)
import Html.Events             exposing (onClick)
import Http
import Model                   exposing (..)
import Task
--------------------------------------------------------------------------------

main = App.program { init          = init
                   , view          = view
                   , update        = update
                   , subscriptions = subscriptions
                   }


-- Model

init : (Model, Cmd Msg)
init = ( Model []
       , Cmd.none
       )


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload               -> (Model [],      getData)
    FetchSuccess newData -> (Model newData, Cmd.none)
    FetchFail    err     -> (Model [],      Cmd.none)

-- View

view : Model -> Html Msg
view model =
  div []
      [ text (toString model.readings)
      , br [] []
      , button [ onClick Reload ] [ text "Reload" ]
      ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- Http

getData : Cmd Msg
getData = let url = "http://sarah-persistent:8080/sensor-readings/date/2016-09-27/room/Livingroom/sensor/Temperature/"
          in Task.perform FetchFail FetchSuccess (Http.get sensorReadings url)
