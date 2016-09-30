module Types
  exposing (..)
--------------------------------------------------------------------------------
import Date        exposing (Date)
import Json.Decode exposing (..)
import Prelude     exposing (..)
import Result      exposing (Result (..))
--------------------------------------------------------------------------------

type Room = Bedroom
          | Livingroom
          | Kitchen
          | Office
          | Bathroom

type Dimension = Temperature
               | Humidity
               | Pressure

type alias Sensor = { room      : Room
                    , dimension : Dimension
                    }

type alias Reading = { date  : Date
                     , time  : Time
                     , value : Float
                     }

type alias SensorReadings = { sensor   : Sensor
                            , readings : List Reading
                            }

--------------------------------------------------------------------------------

room : Decoder Room
room = let decodeToRoom s = case s of
                              "Bedroom"    -> Ok Bedroom
                              "Livingroom" -> Ok Livingroom
                              "Kitchen"    -> Ok Kitchen
                              "Office"     -> Ok Office
                              "Bathroom"   -> Ok Bathroom
                              _            -> Err ("Unknown room: " ++ s)
       in customDecoder string decodeToRoom


dimension : Decoder Dimension
dimension = let decodeToDimension s = case s of
                                        "Temperature" -> Ok Temperature
                                        "Humidity"    -> Ok Humidity
                                        "Pressure"    -> Ok Pressure
                                        _             -> Err ("Unknown dimension: " ++ s)
            in customDecoder string decodeToDimension


date : Decoder Date
date = customDecoder string Date.fromString


sensor : Decoder Sensor
sensor = object2 Sensor room dimension


reading : Decoder Reading
reading = object3 Reading date time float


sensorReadings : Decoder SensorReadings
sensorReadings = object2 SensorReadings sensor (list reading)
