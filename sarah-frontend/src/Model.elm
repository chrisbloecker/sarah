module Model
  exposing (..)
--------------------------------------------------------------------------------
import Date                 exposing (Date)
import Http
import Json.Decode  as Json exposing (Decoder, object5)
import Time                 exposing (Time)
--------------------------------------------------------------------------------

type alias Model = { readings : List SensorReading }

type Msg = Reload
         | FetchSuccess (List SensorReading)
         | FetchFail    Http.Error

type alias SensorReading = { date   : String
                           , time   : String
                           , room   : String
                           , sensor : String
                           , value  : Float
                           }

type Room = Bedroom
          | Livingroom
          | Kitchen
          | Office

type Sensor = Temperature
            | Humidity
            | Pressure

--------------------------------------------------------------------------------

undefined = undefined

room : Decoder Room
room = undefined

sensor : Decoder Sensor
sensor = undefined

sensorReading : Decoder SensorReading
sensorReading = object5 SensorReading Json.string Json.string Json.string Json.string Json.float

sensorReadings : Decoder (List SensorReading)
sensorReadings = Json.list sensorReading
