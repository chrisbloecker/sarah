module Sensors.Update
  exposing (update)
--------------------------------------------------------------------------------
import Sensors.Message exposing (Message)
import Sensors.Model   exposing (Model)
import Util            exposing (..)
import Prelude         exposing (..)
--------------------------------------------------------------------------------

update : Message -> Model -> (Model, Cmd Message)
update = undefined

{-
Reload               -> (model,                                getData model.date Livingroom Temperature)
SetDate      date    -> ({model | date = date},                getData date       Livingroom Temperature)
FetchSuccess newData -> ({model | data = Right newData},       Cmd.none)
FetchFail    err     -> ({model | data = Left (toString err)}, Cmd.none)
-}
