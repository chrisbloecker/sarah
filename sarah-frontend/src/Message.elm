module Message
  exposing (Message (..))
--------------------------------------------------------------------------------
import Home.Message    as Home
import Sensors.Message as Sensors
--------------------------------------------------------------------------------

type Message = HomeMessage    Home.Message
             | SensorsMessage Sensors.Message
