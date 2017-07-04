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
import Data.Text                      (Text, pack, unpack)
import Graphics.UI.Threepenny  hiding (map)
import Graphics.UI.Threepenny.Core    (runFunction, ffi)
import Prelude                 hiding (span, div)
import Sarah.GUI.Reactive
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), embedUI, doNothing)
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand)
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
    RemoteBuilderEnv{..} <- ask
    lift $ do
      liftIO $ putStrLn "Creating display events"
      (eventDisplay, handlerDisplay) <- liftIO newEvent
      (eventMode,    handlerMode)    <- liftIO newEvent

      liftIO $ putStrLn "Creating display behaviours"
      behaviourDisplay <- stepper "foo"    eventDisplay
      behaviourMode    <- stepper "Normal" eventMode

      liftIO $ putStrLn "Creating reactive labels"
      display     <- Material.reactiveLabel behaviourDisplay
      displayMode <- Material.reactiveLabel behaviourMode

      liftIO $ putStrLn "Generating idents"
      getRandomNumberButtonId <- newIdent
      alwaysFailingButtonId   <- newIdent

      liftIO $ putStrLn "Building buttons"
      let getRandomNumberButton = H.button H.! A.class_ (H.toValue . unwords $ [Material.mdl_button, Material.mdl_js_button])
                                           H.! A.id (H.toValue getRandomNumberButtonId) $
                                      Material.icon Material.trending_up
          alwaysFailingButton   = H.button H.! A.class_ (H.toValue . unwords $ [Material.mdl_button, Material.mdl_js_button])
                                           H.! A.id (H.toValue alwaysFailingButtonId) $
                                      Material.icon Material.bug_report

      liftIO $ putStrLn "Constructing event listeners"
      onElementIDClick getRandomNumberButtonId $ liftIO $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.RandomNumberRequest  doNothing (\(ExampleDevice.RandomNumberReply x) -> handlerDisplay (pack . show $ x))
      onElementIDClick alwaysFailingButtonId   $ liftIO $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.AlwaysFailingRequest doNothing (\ExampleDevice.AlwaysFailingReply -> doNothing)

      liftIO $ putStrLn "Creating button events"
      (eventMinusButton, handlerMinusButton) <- liftIO newEvent
      (eventStarButton,  handlerStarButton)  <- liftIO newEvent
      (eventHeartButton, handlerHeartButton) <- liftIO newEvent

      let grey     = ""
          accented = "mdl-color-text--accent"

      liftIO $ putStrLn "Creating button behaviours"
      behaviourMinusButton <- stepper grey eventMinusButton
      behaviourStarButton  <- stepper grey eventStarButton
      behaviourHeartButton <- stepper grey eventHeartButton

      liftIO $ putStrLn "Building buttons"
      minusButton <- Material.reactiveListItem "Normal" behaviourMinusButton
      starButton  <- Material.reactiveListItem "Star"   behaviourStarButton
      heartButton <- Material.reactiveListItem "Heart"  behaviourHeartButton

      liftIO $ putStrLn "Building event handler"
      let eventStateChangedHandler :: Handler (ExampleDevice.DeviceState ExampleDevice)
          eventStateChangedHandler = \case
            ExampleDevice.Normal -> sequence_ [handlerMinusButton accented, handlerStarButton grey,     handlerHeartButton grey,     handlerMode "Normal"]
            ExampleDevice.Star   -> sequence_ [handlerMinusButton grey,     handlerStarButton accented, handlerHeartButton grey,     handlerMode "Star"  ]
            ExampleDevice.Heart  -> sequence_ [handlerMinusButton grey,     handlerStarButton grey,     handlerHeartButton accented, handlerMode "Heart" ]

      -- ToDo: build a handler
      --unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

      liftIO $ putStrLn "Constructing more event listeners"
      onElementIDClick (Material.itemId minusButton) $ liftIO $ do
        putStrLn "[Example.minusButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Normal)

      onElementIDClick (Material.itemId starButton) $ liftIO $ do
        putStrLn "[Example.starButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Star)

      onElementIDClick (Material.itemId heartButton) $ liftIO $ do
        putStrLn "[Example.heartButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Heart)

      liftIO $ putStrLn "Invoke event state changed handler"
      liftIO $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.GetStateRequest doNothing (\(ExampleDevice.GetStateReply state) -> eventStateChangedHandler state)

      liftIO $ putStrLn "Building dropdown"
      dropdown <- Material.dropdown (Material._elementRL displayMode) [Material.item minusButton, Material.item starButton, Material.item heartButton]

      liftIO $ putStrLn "Assembling widget"
      let widget = Material.list [ Material.listItem (Material._elementRL display) getRandomNumberButton
                                 , Material.listItem (H.div $ H.text "") alwaysFailingButton
                                 , Material.listItem (H.label $ H.text "Mode") (Material._elementDropdown dropdown)
                                 ]

      liftIO $ putStrLn "Add widget to TVar"
      liftIO $ atomically . modifyTVar pageTiles $ (widget :)
