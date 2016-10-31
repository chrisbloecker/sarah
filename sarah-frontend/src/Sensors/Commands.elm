module Sensors.Commands
  exposing (..)
--------------------------------------------------------------------------------
import Date  as Date exposing (Date)
import Http
import Model         exposing (..)
import Task
import Util          exposing (..)
--------------------------------------------------------------------------------

getData : Maybe Date -> Room -> Sensor -> Cmd Message
getData mdate room sensor =
  case mdate of
    Nothing -> Cmd.none
    Just date -> let url = "http://192.168.0.7:8080/sensor-readings/"
                        ++ "date/" ++ toIsoDate date
                        ++ "/room/" ++ toString room
                        ++ "/sensor/" ++ toString sensor
                 in Task.perform FetchFail FetchSuccess (Http.get sensorReadings url)

getDate : Cmd Message
getDate = Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now
