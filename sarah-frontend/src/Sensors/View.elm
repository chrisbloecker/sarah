module Sensors.View
  exposing (view)
--------------------------------------------------------------------------------
import Html                   exposing (..)
import Message         as App exposing (Message, sensorMessage)
import Sensors.Message        exposing (..)
import Sensors.Model          exposing (Model)
--------------------------------------------------------------------------------

view : Model -> Html App.Message
view model = div [] [ text "Sensors" ]
