module Sensors.Commands
  exposing (..)
--------------------------------------------------------------------------------
import Date            as Date exposing (Date)
import Http
import Json.Decode             exposing (list)
import Prelude                 exposing (..)
import Sensors.Message         exposing (Message (..))
import Task
import Types                   exposing (..)
import Util                    exposing (..)
--------------------------------------------------------------------------------

getData : Maybe Date -> Room -> Sensor -> Cmd Message
getData mdate room sensor =
  case mdate of
    Nothing -> Cmd.none
    Just date -> let _   = logError ("Loading " ++ url)
                     url = "http://192.168.0.7:8080/sensor-readings"
                        ++ "/date/" ++ toIsoDate date
                        ++ "/room/" ++ toString room
                        ++ "/sensor/" ++ toString sensor

                 in Task.perform (\err -> LoadDataFail (toString err)) LoadDataSuccess (Http.get (list sensorReading) url)

getDate : Cmd Message
getDate = Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now
