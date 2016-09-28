module Main exposing (main)

import Html                   exposing (..)
import Html.App        as App
import Html.Attributes        exposing (..)
import Html.Events            exposing (onClick)
import Http
import Json.Decode as Json
import Task


main = App.program { init          = init
                   , view          = view
                   , update        = update
                   , subscriptions = subscriptions
                   }


-- Model

type alias Model = { readings : String }

init : (Model, Cmd Msg)
init = ( Model "initial"
       , Cmd.none
       )


-- Update

type Msg = Reload
         | FetchSuccess String
         | FetchFail    Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload               -> (Model "Reloading",    getData)
    FetchSuccess newData -> (Model newData,        Cmd.none)
    FetchFail    err     -> (Model (toString err), Cmd.none)


-- View

view : Model -> Html Msg
view model =
  div []
      [ text model.readings
      , br [] []
      , button [ onClick Reload ] [ text "Reload" ]
      ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- Http

getData : Cmd Msg
getData = let url = "http://sarah-persistent:8080/sensor-readings/date/2016-09-27/room/Livingroom/sensor/Temperature/"
          in Task.perform FetchFail FetchSuccess (Http.getString url)
