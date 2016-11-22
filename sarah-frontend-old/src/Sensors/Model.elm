module Sensors.Model
  exposing ( Model
           , init
           )
--------------------------------------------------------------------------------
import Date             as Data exposing (Date)
import Sensors.Commands         exposing (getDate)
import Sensors.Message          exposing (Message)
import Types                    exposing (..)
--------------------------------------------------------------------------------

type alias Model = { date : Maybe Date
                   , data : List SensorReading
                   }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = ( { date = Nothing
         , data = []
         }
       , getDate
       )
