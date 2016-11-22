module Update
  exposing (..)
--------------------------------------------------------------------------------
import Message                   exposing (..)
import Model                     exposing (..)
import Platform.Cmd              exposing (batch)
import Ports                     exposing (..)
import Types                     exposing (..)
--------------------------------------------------------------------------------
import Home.Update    as Home
import Sensors.Update as Sensors
--------------------------------------------------------------------------------

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case msg of
    HomeMessage m ->
      let (home', cmd) = Home.update m model.home
      in ({ model | home = home' }, Cmd.map HomeMessage cmd)

    SensorsMessage m ->
      let (sensors', cmd) = Sensors.update m model.sensors
      in ( { model | sensors = sensors' }
         , Cmd.map SensorsMessage cmd
         )

    Display page ->
      let config' = model.config
      in ( { model | config = { config' | display = page } }
         , case page of
             PageHome    -> Cmd.none
             PageSensors -> Cmd.none
         )
