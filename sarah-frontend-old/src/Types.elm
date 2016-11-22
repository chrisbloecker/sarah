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

type Sensor = Temperature
            | Humidity
            | Pressure

type alias SensorReading = { date   : Date
                           , time   : Time
                           , room   : Room
                           , sensor : Sensor
                           , value  : Float
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


sensor : Decoder Sensor
sensor = let decodeToSensor s = case s of
                                  "Temperature" -> Ok Temperature
                                  "Humidity"    -> Ok Humidity
                                  "Pressure"    -> Ok Pressure
                                  _             -> Err ("Unknown sensor: " ++ s)
            in customDecoder string decodeToSensor


date : Decoder Date
date = customDecoder string Date.fromString

sensorReading : Decoder SensorReading
sensorReading = object5 SensorReading
                        ("date"   := date)
                        ("time"   := time)
                        ("room"   := room)
                        ("sensor" := sensor)
                        ("value"  := float)
