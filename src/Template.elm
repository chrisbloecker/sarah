module Template
  exposing (template)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Attributes exposing (..)
import Message         exposing (Message)
--------------------------------------------------------------------------------

template : List (Html Message) -> Html Message
template content =
  let navbar = nav [ class "navbar navbar-light bg-faded" ]
                   [ ul [ class "nav navbar-nav" ]
                        [ li [ class "nav-item" ]
                             [ a [ class "nav-link", href "https://google.com" ]
                                 [ text "Home" ]
                             ]
                        ]
                   ]
  in div [ class "container" ]
         (navbar :: content)
