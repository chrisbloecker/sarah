module Template
  exposing (template)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Attributes exposing (..)
import Message         exposing (Message)
--------------------------------------------------------------------------------

template : List (Html Message) -> Html Message
template content = div [ class "container" ] content
