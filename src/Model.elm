module Model
  exposing (..)
--------------------------------------------------------------------------------
import Date                     exposing (Date)
import Http
import Message                  exposing (Message (..))
import Platform.Cmd             exposing (batch, map)
import Types                    exposing (..)
--------------------------------------------------------------------------------
import Home.Model    as Home
import Sensors.Model as Sensors
--------------------------------------------------------------------------------

type alias Model = { home    : Home.Model
                   , sensors : Sensors.Model
                   }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = let (initHome, cmdHome)       = Home.init
           (initSensors, cmdSensors) = Sensors.init
       in ( { home    = initHome
            , sensors = initSensors
            }
          , batch [ map HomeMessage    cmdHome
                  , map SensorsMessage cmdSensors
                  ]
          )
