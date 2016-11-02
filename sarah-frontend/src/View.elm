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
view model = template model.config.display [ case model.config.display of
                                               PageHome    -> div [] []
                                               PageSensors -> Sensors.view model.sensors
--                      , button [ onClick SendToJS ]
--                               [ text "ping js" ]
                                    ]
