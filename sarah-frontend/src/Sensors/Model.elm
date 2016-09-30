module Sensors.Model
  exposing ( Model
           , init
           )
--------------------------------------------------------------------------------
import Sensors.Message exposing (Message)
import Types           exposing (..)
--------------------------------------------------------------------------------

type alias Model = { data : List SensorReadings }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = ( { data = [] }
       , Cmd.none
       )
