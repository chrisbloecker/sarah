module View
  exposing (view)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Message         exposing (Message (..))
import Model           exposing (Model)
import Prelude         exposing (..)
import Template        exposing (template)
--------------------------------------------------------------------------------

view : Model -> Html Message
view model = template [ div [ class "text-center" ]
                            [ text "Nothing here yet..." ]
                      , button [ onClick SendToJS ]
                               [ text "ping js" ]
                      ]
