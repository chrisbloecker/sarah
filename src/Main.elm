module Main
  exposing (main)
--------------------------------------------------------------------------------
import Html.App as App
import Message         exposing (Message)
import Model           exposing (Model, init)
import Prelude         exposing (..)
import Update          exposing (update)
import View            exposing (view)
--------------------------------------------------------------------------------

main : Program Never
main = App.program { init          = init
                   , view          = view
                   , update        = update
                   , subscriptions = subscriptions
                   }

subscriptions : Model -> Sub Message
subscriptions _ = Sub.none
