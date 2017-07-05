{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Concurrent.STM         (atomically, modifyTVar)
import Control.Monad                  (void)
import Control.Monad.Reader           (lift, ask, runReaderT)
import Data.Foldable                  (traverse_)
import Data.Text                      (Text, pack, unpack, unwords)
import Graphics.UI.Threepenny  hiding (map)
import Graphics.UI.Threepenny.Core    (runFunction, ffi)
import Prelude                 hiding (span, div, unwords)
import Sarah.GUI.Reactive
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), doNothing, addPageTile, addPageAction)
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device        (ExampleDevice)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Material            as Material
import qualified Sarah.Middleware.Device.Example as ExampleDevice
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  buildRemote _ = do
    env@RemoteBuilderEnv{..} <- ask

    (eventDisplay, handlerDisplay) <- liftIO newEvent
    (eventMode,    handlerMode)    <- liftIO newEvent

    behaviourDisplay <- stepper "foo"    eventDisplay
    behaviourMode    <- stepper "Normal" eventMode

    display     <- lift $ Material.reactiveLabel behaviourDisplay
    displayMode <- lift $ Material.reactiveLabel behaviourMode

    getRandomNumberButtonId <- newIdent
    alwaysFailingButtonId   <- newIdent

    let getRandomNumberButton = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                                         H.! A.id (H.toValue getRandomNumberButtonId) $
                                             Material.icon Material.trending_up
        alwaysFailingButton   = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                                         H.! A.id (H.toValue alwaysFailingButtonId) $
                                             Material.icon Material.bug_report

    addPageAction $
      onElementIDClick getRandomNumberButtonId $ liftIO $ flip runReaderT remoteRunnerEnv $
        withResponse ExampleDevice.RandomNumberRequest
          doNothing
          (\(ExampleDevice.RandomNumberReply x) -> handlerDisplay (pack . show $ x))

    addPageAction $
      onElementIDClick alwaysFailingButtonId $ liftIO $ flip runReaderT remoteRunnerEnv $
        withResponse ExampleDevice.AlwaysFailingRequest
          doNothing
          (\ExampleDevice.AlwaysFailingReply -> doNothing)

    (eventNormalButton, handlerNormalButton) <- liftIO newEvent
    (eventStarButton,   handlerStarButton)   <- liftIO newEvent
    (eventHeartButton,  handlerHeartButton)  <- liftIO newEvent

    let grey     = ""
        accented = "mdl-color-text--accent"

    behaviourNormalButton <- stepper accented eventNormalButton
    behaviourStarButton   <- stepper grey     eventStarButton
    behaviourHeartButton  <- stepper grey     eventHeartButton

    (normalButton, normalButtonId) <- lift $ Material.reactiveListItem "Normal" behaviourNormalButton
    (starButton,   starButtonId)   <- lift $ Material.reactiveListItem "Star"   behaviourStarButton
    (heartButton,  heartButtonId)  <- lift $ Material.reactiveListItem "Heart"  behaviourHeartButton

    let eventStateChangedHandler :: Handler (ExampleDevice.DeviceState ExampleDevice)
        eventStateChangedHandler = \case
          ExampleDevice.Normal -> sequence_ [handlerNormalButton accented, handlerStarButton grey,     handlerHeartButton grey,     handlerMode "Normal"]
          ExampleDevice.Star   -> sequence_ [handlerNormalButton grey,     handlerStarButton accented, handlerHeartButton grey,     handlerMode "Star"  ]
          ExampleDevice.Heart  -> sequence_ [handlerNormalButton grey,     handlerStarButton grey,     handlerHeartButton accented, handlerMode "Heart" ]

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick normalButtonId $ liftIO $ do
        putStrLn "[Example.minusButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Normal)

    addPageAction $
      onElementIDClick starButtonId $ liftIO $ do
        putStrLn "[Example.starButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Star)

    addPageAction $
      onElementIDClick heartButtonId $ liftIO $ do
        putStrLn "[Example.heartButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Heart)

    liftIO $ flip runReaderT remoteRunnerEnv $
      withResponse ExampleDevice.GetStateRequest
        doNothing
        (\(ExampleDevice.GetStateReply state) -> eventStateChangedHandler state)

    dropdown <- lift $ Material.dropdown displayMode [normalButton, starButton, heartButton]

    let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
        widget = Material.mkTile title $
                     Material.list [ Material.listItem display getRandomNumberButton
                                   , Material.listItem (H.div $ H.text "") alwaysFailingButton
                                   , Material.listItem (H.label $ H.text "Mode") dropdown
                                   ]

    addPageTile widget
