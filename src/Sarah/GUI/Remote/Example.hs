{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Monad                  (void)
import Control.Monad.Reader           (lift, ask, runReaderT)
import Data.Foldable                  (traverse_)
import Data.Text                      (Text, unpack)
import Graphics.UI.Threepenny  hiding (map)
import Graphics.UI.Threepenny.Core    (runFunction, ffi)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), embedUI, doNothing)
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand)
import Sarah.Middleware.Device        (ExampleDevice)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Material            as Material
import qualified Sarah.Middleware.Device.Example as ExampleDevice
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      (eventDisplay, handlerDisplay) <- liftIO newEvent
      (eventMode,    handlerMode)    <- liftIO newEvent

      behaviourDisplay <- stepper "foo"    eventDisplay
      behaviourMode    <- stepper "Normal" eventMode

      display     <- Material.reactiveLabel behaviourDisplay
      displayMode <- Material.reactiveLabel behaviourMode

      getRandomNumberButton <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.trending_up ]
      alwaysFailingButton   <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.bug_report  ]

      on click getRandomNumberButton $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.RandomNumberRequest  doNothing (\(ExampleDevice.RandomNumberReply x) -> handlerDisplay $ show x)
      on click alwaysFailingButton   $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.AlwaysFailingRequest doNothing (\ExampleDevice.AlwaysFailingReply -> doNothing)

      (eventMinusButton, handlerMinusButton) <- liftIO newEvent
      (eventStarButton,  handlerStarButton)  <- liftIO newEvent
      (eventHeartButton, handlerHeartButton) <- liftIO newEvent

      let grey     = Material.empty
          accented = Material.mdl_color_text_accent

      behaviourMinusButton  <- stepper grey eventMinusButton
      behaviourStarButton   <- stepper grey eventStarButton
      behaviourHeartButton  <- stepper grey eventHeartButton

      minusButton <- Material.reactiveListItem behaviourMinusButton
      starButton  <- Material.reactiveListItem behaviourStarButton
      heartButton <- Material.reactiveListItem behaviourHeartButton

      element (getElement minusButton) # set text "Normal"
      element (getElement starButton)  # set text "Star"
      element (getElement heartButton) # set text "Heart"

      let eventStateChangedHandler :: Handler (ExampleDevice.DeviceState ExampleDevice)
          eventStateChangedHandler = \case
            ExampleDevice.Normal -> sequence_ [handlerMinusButton accented, handlerStarButton grey,     handlerHeartButton grey,     handlerMode "Normal"]
            ExampleDevice.Star   -> sequence_ [handlerMinusButton grey,     handlerStarButton accented, handlerHeartButton grey,     handlerMode "Star"  ]
            ExampleDevice.Heart  -> sequence_ [handlerMinusButton grey,     handlerStarButton grey,     handlerHeartButton accented, handlerMode "Heart" ]

      unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

      on click (getElement minusButton) $ embedUI $ do
        putStrLn "[Example.minusButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Normal)

      on click (getElement starButton)  $ embedUI $ do
        putStrLn "[Example.starButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Star)

      on click (getElement heartButton) $ embedUI $ do
        putStrLn "[Example.heartButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Heart)

      liftIO $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.GetStateRequest doNothing (\(ExampleDevice.GetStateReply state) -> eventStateChangedHandler state)

      dropdown <- Material.dropdown (element displayMode) [element minusButton, element starButton, element heartButton]

      getElement <$> Material.list [ Material.listItem (element display) (element getRandomNumberButton)
                                   , Material.listItem div (element alwaysFailingButton)
                                   , Material.listItem (label # set text "Mode") (element dropdown)
                                   ]
