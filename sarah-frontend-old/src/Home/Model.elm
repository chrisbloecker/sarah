module Home.Model
  exposing ( Model
           , init
           )
--------------------------------------------------------------------------------
import Home.Message exposing (Message)
--------------------------------------------------------------------------------

type alias Model = { data : Maybe String }

--------------------------------------------------------------------------------

init : (Model, Cmd Message)
init = ( { data = Nothing }
       , Cmd.none
       )
