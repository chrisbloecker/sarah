module Sensors.Update
  exposing (update)
--------------------------------------------------------------------------------
import Sensors.Commands exposing (getData)
import Sensors.Message  exposing (Message (..))
import Sensors.Model    exposing (Model)
import Util             exposing (..)
import Prelude          exposing (..)
import Types            exposing (Room (..), Sensor (..))
--------------------------------------------------------------------------------

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    SetDate         date -> ({ model | date = date }, getData date Livingroom Temperature)
    LoadData             -> (model, getData model.date Livingroom Temperature)
    LoadDataFail    err  ->
      let _ = logError err
      in (model, Cmd.none)
    LoadDataSuccess data -> ({ model | data = data }, Cmd.none)
