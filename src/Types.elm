module Types
  exposing (..)
--------------------------------------------------------------------------------
import Json.Decode     exposing (Decoder, (:=), list, float, string, object5)
--------------------------------------------------------------------------------

type Room = Bedroom
          | Livingroom
          | Kitchen
          | Office

type Sensor = Temperature
            | Humidity
            | Pressure

type alias SensorReading = { date   : String
                           , time   : String
                           , room   : String
                           , sensor : String
                           , value  : Float
                           }

--------------------------------------------------------------------------------

sensorReading : Decoder SensorReading
sensorReading = object5 SensorReading
                  ("date"   := string)
                  ("time"   := string)
                  ("room"   := string)
                  ("sensor" := string)
                  ("value"  := float)

sensorReadings : Decoder (List SensorReading)
sensorReadings = list sensorReading
