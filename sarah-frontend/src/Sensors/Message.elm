module Sensors.Message
  exposing (Message (..))
--------------------------------------------------------------------------------
import Date  as Date exposing (Date)
import Types         exposing (SensorReading)
--------------------------------------------------------------------------------

type Message = SetDate (Maybe Date)
             | LoadData
             | LoadDataFail
             | LoadDataSuccess (List SensorReading)
