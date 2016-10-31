module Template
  exposing (template)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Message         exposing (..)
--------------------------------------------------------------------------------

template : List (Html Message) -> Html Message
template content =
  let navbar = nav [ class "navbar navbar-light bg-faded" ]
                   [ ul [ class "nav navbar-nav" ]
                        [ li [ class "nav-item" ]
                             [ a [ class "nav-link"
                                 , onClick (Display PageHome)
                                 , href "#"
                                 ]
                                  [ text "Home" ]
                             ]
                        , li [ class "nav-item" ]
                             [ a [ class "nav-link"
                                 , onClick (Display PageSensors)
                                 , href "#"
                                 ]
                                 [ text "Sensors" ]
                             ]
                        ]
                   ]
  in div [ class "container" ]
         (navbar :: content)
