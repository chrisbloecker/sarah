module Template
  exposing (template)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Message         exposing (..)
import Prelude         exposing ((.))
--------------------------------------------------------------------------------

active : Page -> Page -> String -> String
active activePage page str = if page == activePage then
                               "active " ++ str
                             else
                               str

template : Page -> List (Html Message) -> Html Message
template activePage content =
  let navbar = nav [ class "navbar navbar-dark bg-inverse" ]
                   [ ul [ class "nav navbar-nav" ]
                        [ li [ class . active activePage PageHome <| "nav-item" ]
                             [ a [ class "nav-link"
                                 , onClick (Display PageHome)
                                 , href "#"
                                 ]
                                  [ text "Home" ]
                             ]
                        , li [ class . active activePage PageSensors <| "nav-item" ]
                             [ a [ class "nav-link"
                                 , onClick (Display PageSensors)
                                 , href "#"
                                 ]
                                 [ text "Sensors" ]
                             ]
                        , li [ class . active activePage PageLog <| "nav-item" ]
                             [ a [ class "nav-link"
                                 , onClick (Display PageLog)
                                 , href "#"
                                 ]
                                 [ text "Log" ]
                             ]
                        ]
                   ]
  in div [ class "container" ]
         (navbar :: content)
