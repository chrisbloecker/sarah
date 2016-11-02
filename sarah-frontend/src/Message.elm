module Message
  exposing (..)
--------------------------------------------------------------------------------
import Home.Message    as Home
import Log.Message     as Log
import Sensors.Message as Sensors
--------------------------------------------------------------------------------

type Message = HomeMessage    Home.Message
             | LogMessage     Log.Message
             | SensorsMessage Sensors.Message
             | Display        Page

type Page = PageHome
          | PageLog
          | PageSensors

--------------------------------------------------------------------------------

sensorMessage : Sensors.Message -> Message
sensorMessage = SensorsMessage
