{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Monad                       (forM)
import Control.Monad.Reader                (lift, ask)
import Control.Monad.IO.Class              (liftIO)
import Data.Foldable                       (traverse_)
import Data.Text                           (unwords, pack)
import Graphics.UI.Material
import Graphics.UI.Threepenny              (Handler, register)
import Prelude                      hiding (unwords)
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket                 (withResponse, withoutResponse)
import Sarah.Middleware                    (DeviceState, decodeDeviceState, DeviceAddress (..), Schedule (..))
import Sarah.Middleware.Device.Power.HS110
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

instance HasRemote HS110 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    powerSwitch <- lift $ reactiveToggle False

    let eventStateChangedHandler :: Handler (DeviceState HS110)
        eventStateChangedHandler HS110State{..} = getHandler powerSwitch isOn

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDCheckedChange (getItemId powerSwitch) $ \state -> runRemote $
        withoutResponse $
          if state
            then PowerOn
            else PowerOff

    addPageTile $
      let title = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img   = Nothing -- Just "static/img/remote/power.png"
      in mkTile3 title img $ list [ listItem (H.text "Power") $ getItem powerSwitch ]

    -- get the state of the device
    addPageAction $
      runRemote $
        withResponse GetStateRequest
        doNothing
        (\(GetStateReply state) -> eventStateChangedHandler state)


  buildSchedule _ = do
    ScheduleBuilderEnv{..} <- ask
    schedule               <- getSchedule

    let input = H.div H.! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
                    H.select H.! A.class_ "mdl-textfield__input"
                             H.! A.id "octane"
                             H.! A.name "octane" $ do
                        H.option $
                            H.text ""
                        H.option H.! A.value "85" $
                            H.text "85"
                        H.option H.! A.value "87" $
                            H.text "87"
                    H.label H.! A.class_ "mdl-textfield__label"
                            H.! A.for "octane" $
                        H.text "Octane"

    addItemButton   <- button Nothing (Just "Add")
    addItemDialogue <- dialogue "Add schedule" input

    -- display the dialogue to add a schedule item
    addPageAction $
      onElementIDClick (getItemId addItemButton) $
        showDialogue (getItemId addItemDialogue)

    -- submitting the new schedule item through the dialogue
    --addPageAction $
    --  onElementIDClick (getSubmitButton addItemDialogue) $
    --    return ()

    -- hide the dialogue
    -- ToDo: should we reset the input elements?
    addPageAction $
      onElementIDClick (getDismissButtonId addItemDialogue) $
        hideDialogue (getItemId addItemDialogue)

    -- we have to add the dialogue to the page
    addPageTile $
      getItem addItemDialogue

    -- and the tile for the device
    addPageTile $
      let title         = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img           = Nothing
          scheduleItems = map (\Schedule{..} -> listItem (H.text . pack . show $ scheduleTimer) (H.text "")) schedule
      in mkTile3 title img (list $ scheduleItems ++ [getItem addItemButton])
