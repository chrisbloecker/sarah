module Sensors.Model
  exposing ( Model
           , init
           )
--------------------------------------------------------------------------------
import Dict            exposing (Dict, empty)
import Sensors.Message exposing (Message)
import Types           exposing (..)
--------------------------------------------------------------------------------

type alias Model = { data : Dict (Room, Sensor) (List SensorReading) }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = ( { data = empty }
       , Cmd.none
       )
