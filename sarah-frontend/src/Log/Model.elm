module Log.Model
  exposing (Model, init)
--------------------------------------------------------------------------------
import Log.Message exposing (Message)
--------------------------------------------------------------------------------

type alias Model = { logs : List String }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = ( { logs = [ "App started" ] }
       , Cmd.none
       )
