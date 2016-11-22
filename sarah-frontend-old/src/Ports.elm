port module Ports
  exposing (..)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type alias SensorReadingsRenderConfig = { values : List Float }

--------------------------------------------------------------------------------

port renderSensorReadings : SensorReadingsRenderConfig -> Cmd msg
