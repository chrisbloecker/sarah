module Sensors.Commands
  exposing (..)
--------------------------------------------------------------------------------
import Date            as Date exposing (Date)
import Http
import Model                   exposing (..)
import Sensors.Message         exposing (Message (..))
import Task
import Types                   exposing (..)
import Util                    exposing (..)
--------------------------------------------------------------------------------

getData : Maybe Date -> Sensor -> Cmd Message
getData mdate sensor =
  case mdate of
    Nothing -> Cmd.none
    Just date -> let url = "http://192.168.0.7:8080/sensor-readings/"
                        ++ "date/" ++ toIsoDate date
                        ++ "/room/" ++ toString sensor.room
                        ++ "/sensor/" ++ toString sensor.dimension
                 in Task.perform (always LoadDataFail) LoadDataSuccess (Http.get sensorReadings url)

getDate : Cmd Message
getDate = Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now
