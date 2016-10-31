module Message
  exposing (..)
--------------------------------------------------------------------------------
import Home.Message    as Home
import Sensors.Message as Sensors
--------------------------------------------------------------------------------

type Message = HomeMessage    Home.Message
              | SensorsMessage Sensors.Message
              | Display        Page
              | SendToJS

type Page = PageHome
          | PageSensors

sensorMessage : Sensors.Message -> Message
sensorMessage = SensorsMessage
