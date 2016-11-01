module Sensors.Message
  exposing (Message (..))
--------------------------------------------------------------------------------
import Date  as Date exposing (Date)
import Types         exposing (SensorReading)
--------------------------------------------------------------------------------

type Message = SetDate         (Maybe Date)
             | LoadData
             | LoadDataFail    String
             | LoadDataSuccess (List SensorReading)
