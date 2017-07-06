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
import Graphics.UI.Material
import Graphics.UI.Threepenny  hiding (map)
import Graphics.UI.Threepenny.Core    (runFunction, ffi)
import Prelude                 hiding (span, div, unwords)
import Sarah.GUI.Reactive
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), RemoteRunner, doNothing, addPageTile, addPageAction)
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device        (ExampleDevice)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Example as ExampleDevice
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  buildRemote _ = do
    env@RemoteBuilderEnv{..} <- ask

    display <- lift $ reactiveLabel "foo"
    mode    <- lift $ reactiveLabel "Normal"

    getRandomNumberButtonId <- newIdent
    alwaysFailingButtonId   <- newIdent

    let getRandomNumberButton = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                                         H.! A.id (H.toValue getRandomNumberButtonId) $
                                             icon trending_up
        alwaysFailingButton   = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                                         H.! A.id (H.toValue alwaysFailingButtonId) $
                                             icon bug_report

    addPageAction $
      onElementIDClick getRandomNumberButtonId $ runRemote $
        withResponse ExampleDevice.RandomNumberRequest
          doNothing
          (\(ExampleDevice.RandomNumberReply x) -> getHandler display (pack . show $ x))

    addPageAction $
      onElementIDClick alwaysFailingButtonId $ runRemote $
        withResponse ExampleDevice.AlwaysFailingRequest
          doNothing
          (\ExampleDevice.AlwaysFailingReply -> doNothing)

    let grey     = ""
        accented = "mdl-color-text--accent"

    normalButton <- lift $ reactiveListItem "Normal" accented
    starButton   <- lift $ reactiveListItem "Star"   grey
    heartButton  <- lift $ reactiveListItem "Heart"  grey

    let eventStateChangedHandler :: Handler (ExampleDevice.DeviceState ExampleDevice)
        eventStateChangedHandler = \case
          ExampleDevice.Normal -> sequence_ [getHandler normalButton accented, getHandler starButton grey,     getHandler heartButton grey,     getHandler mode ("Normal" :: Text)]
          ExampleDevice.Star   -> sequence_ [getHandler normalButton grey,     getHandler starButton accented, getHandler heartButton grey,     getHandler mode ("Star"   :: Text)]
          ExampleDevice.Heart  -> sequence_ [getHandler normalButton grey,     getHandler starButton grey,     getHandler heartButton accented, getHandler mode ("Heart"  :: Text)]

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick (getItemId normalButton) $ do
        liftIO $ putStrLn "[Example.minusButton.click]"
        runRemote $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Normal)

    addPageAction $
      onElementIDClick (getItemId starButton) $ do
        liftIO $ putStrLn "[Example.starButton.click]"
        runRemote $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Star)

    addPageAction $
      onElementIDClick (getItemId heartButton) $ do
        liftIO $ putStrLn "[Example.heartButton.click]"
        runRemote $ withoutResponse (ExampleDevice.SetStateRequest ExampleDevice.Heart)

    dropdown <- lift $ dropdown (getItem mode) [ getItem normalButton
                                               , getItem starButton
                                               , getItem heartButton
                                               ]

    addPageTile $
      let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
      in mkTile title $ list [ listItem (getItem display) getRandomNumberButton
                             , listItem (H.div $ H.text "") alwaysFailingButton
                             , listItem (H.label $ H.text "Mode") (getItem dropdown)
                             ]

    -- get the current state and set it
    lift $ runRemote $
      withResponse ExampleDevice.GetStateRequest
        doNothing
        (\(ExampleDevice.GetStateReply state) -> eventStateChangedHandler state)
