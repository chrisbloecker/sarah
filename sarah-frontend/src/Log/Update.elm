module Log.Update
  exposing (update)
--------------------------------------------------------------------------------
import Log.Message exposing (..)
import Log.Model   exposing (Model)
--------------------------------------------------------------------------------

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Log log -> ({ model | logs = log :: model.logs }, Cmd.none)
