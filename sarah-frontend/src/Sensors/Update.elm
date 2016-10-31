module Sensors.Update
  exposing (update)
--------------------------------------------------------------------------------
import Sensors.Commands exposing (getData)
import Sensors.Message  exposing (Message (..))
import Sensors.Model    exposing (Model)
import Util             exposing (..)
import Prelude          exposing (..)
import Types exposing (Room (..), Dimension (..))
--------------------------------------------------------------------------------

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    SetDate         date -> undefined
    LoadData             -> (model, getData model.date { room = Livingroom, dimension = Temperature })
    LoadDataFail         -> undefined
    LoadDataSuccess data -> undefined
