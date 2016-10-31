module Log.View
  exposing (view)
--------------------------------------------------------------------------------
import Html            exposing (..)
import Html.Attributes exposing (class)
import Message as App  exposing (Message)
import Log.Model       exposing (Model)
--------------------------------------------------------------------------------

view : Model -> Html App.Message
view model = div [ class "text-center" ]
                 [ text "Log"
                 , table [ class "table table-hover table-striped" ]
                         [ thead []
                                 [ tr []
                                      [ th []
                                           [ text "Message"
                                           ]
                                      ]
                                 ]
                         , tbody [] (List.map row model.logs)
                         ]
                 ]

row : String -> Html App.Message
row log = tr [] [ text log ]
