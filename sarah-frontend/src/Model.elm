module Model
  exposing (..)
--------------------------------------------------------------------------------
import Date                     exposing (Date)
import Http
import Message                  exposing (..)
import Platform.Cmd             exposing (batch, map)
import Types                    exposing (..)
--------------------------------------------------------------------------------
import Home.Model    as Home
import Sensors.Model as Sensors
--------------------------------------------------------------------------------

type alias Config = { middleware : String
                    , display    : Page
                    }

type alias Model = { config  : Config
                   , home    : Home.Model
                   , sensors : Sensors.Model
                   }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = let (initHome,    cmdHome)    = Home.init
           (initSensors, cmdSensors) = Sensors.init
       in ( { config  = { middleware = "192.168.0.7:8080"
                        , display    = PageHome
                        }
            , home    = initHome
            , sensors = initSensors
            }
          , batch [ map HomeMessage    cmdHome
                  , map SensorsMessage cmdSensors
                  ]
          )
