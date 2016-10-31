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
import Log.Model     as Log
import Sensors.Model as Sensors
--------------------------------------------------------------------------------

type alias Config = { middleware : String
                    , display    : Page
                    }

type alias Model = { config  : Config
                   , home    : Home.Model
                   , log     : Log.Model
                   , sensors : Sensors.Model
                   }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = let (initHome,    cmdHome)    = Home.init
           (initLog,     cmdLog)     = Log.init
           (initSensors, cmdSensors) = Sensors.init
       in ( { config  = { middleware = "192.168.0.7:8080"
                        , display    = PageHome
                        }
            , home    = initHome
            , log     = initLog
            , sensors = initSensors
            }
          , batch [ map HomeMessage    cmdHome
                  , map LogMessage     cmdLog
                  , map SensorsMessage cmdSensors
                  ]
          )
