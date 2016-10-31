module View
  exposing (view)
--------------------------------------------------------------------------------
import Html                    exposing (..)
import Html.Events             exposing (..)
import Html.Attributes         exposing (..)
import Message                 exposing (..)
import Model                   exposing (Model)
import Prelude                 exposing (..)
import Template                exposing (template)
--------------------------------------------------------------------------------
import Sensors.View as Sensors exposing (view)
--------------------------------------------------------------------------------

view : Model -> Html Message
view model = template [ case model.config.display of
                          PageHome    -> div [] [ text "Home" ]
                          PageSensors -> Sensors.view model.sensors
--                      , button [ onClick SendToJS ]
--                               [ text "ping js" ]
                      ]
