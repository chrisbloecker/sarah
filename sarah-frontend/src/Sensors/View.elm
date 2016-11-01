module Sensors.View
  exposing (view)
--------------------------------------------------------------------------------
import Html                   exposing (Html, div, text)
import Html.Attributes        exposing (id)
import Message         as App exposing (Message, sensorMessage)
import Sensors.Message        exposing (Message)
import Sensors.Model          exposing (Model)
--------------------------------------------------------------------------------

view : Model -> Html App.Message
view model = div [] [ div [ id "plot" ]
                          []
                    ]
