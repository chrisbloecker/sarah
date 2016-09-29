module Main exposing (main)
--------------------------------------------------------------------------------
import Chart                   exposing (lChart, toHtml, title)
import Date            as Date exposing (Date, Month (..), year, day)
import Html                    exposing (..)
import Html.App        as App
import Html.Attributes         exposing (..)
import Html.Events             exposing (onClick)
import Http
import List                    exposing (map)
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
init = ( Model (Right []) Nothing
       , getDate
       )


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload               -> (model,                                getData model.date Livingroom Temperature)
    SetDate      date    -> ({model | date = date},                getData date       Livingroom Temperature)
    FetchSuccess newData -> ({model | data = Right newData},       Cmd.none)
    FetchFail    err     -> ({model | data = Left (toString err)}, Cmd.none)

-- View

view : Model -> Html Msg
view model =
  case model.data of
    Left err       -> text err
    Right readings -> div [] [ lChart (map (\r -> (r.value, "")) readings)
                                 |> Chart.title (toString model.date)
                                 |> toHtml
                             , br [] []
                             , button [ onClick Reload ] [ text "Reload" ]
                             ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- Http

getData : Maybe Date -> Room -> Sensor -> Cmd Msg
getData mdate room sensor =
  case mdate of
    Nothing -> Cmd.none
    Just date -> let url = "http://192.168.0.7:8080/sensor-readings/"
                        ++ "date/" ++ toIsoDate date
                        ++ "/room/" ++ toString room
                        ++ "/sensor/" ++ toString sensor
                 in Task.perform FetchFail FetchSuccess (Http.get sensorReadings url)

getDate : Cmd Msg
getDate = Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now

toIsoDate : Date -> String
toIsoDate date = let yyyy = toString (year date)
                     mm   = month (Date.month date)
                     dd   = toString (day date)
                 in yyyy ++ "-" ++ mm ++ "-" ++ dd

month : Month -> String
month m = case m of
  Jan -> "01"
  Feb -> "02"
  Mar -> "03"
  Apr -> "04"
  May -> "05"
  Jun -> "06"
  Jul -> "07"
  Aug -> "08"
  Sep -> "09"
  Oct -> "10"
  Nov -> "11"
  Dec -> "12"
