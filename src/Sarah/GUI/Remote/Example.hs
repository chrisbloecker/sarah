{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Reader            (lift, ask)
import Data.Foldable                   (traverse_)
import Data.Text                       (Text, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny          (ToJS (..), Handler, register)
import Prelude                  hiding (unwords)
import Sarah.GUI.Reactive
import Sarah.GUI.Model                 (HasRemote (..), RemoteBuilderEnv (..), doNothing, addPageTile, addPageAction)
import Sarah.GUI.Websocket             (withResponse, withoutResponse)
import Sarah.Middleware                (decodeDeviceState, DeviceAddress (..))
import Sarah.Middleware.Device.Example
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  buildRemote _ = do
    env@RemoteBuilderEnv{..} <- ask

    display <- lift $ reactiveLabel "foo"
    mode    <- lift $ reactiveLabel "Normal"

    getRandomNumberButton <- iconButton trending_up
    alwaysFailingButton   <- iconButton bug_report

    addPageAction $
      onElementIDClick (getItemId getRandomNumberButton) $ runRemote $ do
        liftIO $ putStrLn "[Example.getRandomNumberButton.click]"
        withResponse RandomNumberRequest
          doNothing
          (\(RandomNumberReply x) -> getHandler display (pack . show $ x))

    addPageAction $
      onElementIDClick (getItemId alwaysFailingButton) $ runRemote $ do
        liftIO $ putStrLn "[Example.alwaysFailingButton.click]"
        withResponse AlwaysFailingRequest
          doNothing
          (\AlwaysFailingReply -> doNothing)

    let grey     = ""
        accented = "mdl-color-text--accent"

    normalButton <- lift $ reactiveListItem "Normal" accented
    starButton   <- lift $ reactiveListItem "Star"   grey
    heartButton  <- lift $ reactiveListItem "Heart"  grey

    normalOption <- lift $ reactiveOption Normal
    starOption   <- lift $ reactiveOption Star
    heartOption  <- lift $ reactiveOption Heart

    selectField  <- lift $ reactiveSelectField [normalOption, starOption, heartOption] ("Normal" :: Text)

    let eventStateChangedHandler :: Handler (DeviceState ExampleDevice)
        eventStateChangedHandler = \case
          Normal -> sequence_ [getHandler normalButton accented, getHandler starButton grey,     getHandler heartButton grey,     getHandler mode ("Normal" :: Text), getHandler selectField ("Normal" :: Text)]
          Star   -> sequence_ [getHandler normalButton grey,     getHandler starButton accented, getHandler heartButton grey,     getHandler mode ("Star"   :: Text), getHandler selectField ("Star"   :: Text)]
          Heart  -> sequence_ [getHandler normalButton grey,     getHandler starButton grey,     getHandler heartButton accented, getHandler mode ("Heart"  :: Text), getHandler selectField ("Heart"  :: Text)]

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick (getItemId normalButton) $ do
        liftIO $ putStrLn "[Example.normalButton.click]"
        runRemote $ withoutResponse (SetStateRequest Normal)

    addPageAction $
      onElementIDClick (getItemId starButton) $ do
        liftIO $ putStrLn "[Example.starButton.click]"
        runRemote $ withoutResponse (SetStateRequest Star)

    addPageAction $
      onElementIDClick (getItemId heartButton) $ do
        liftIO $ putStrLn "[Example.heartButton.click]"
        runRemote $ withoutResponse (SetStateRequest Heart)

    addPageAction $
      onElementIDChange (getItemId selectField) $ \newOption -> do
        liftIO $ putStrLn "[Example.selectField.change]"
        runRemote $ withoutResponse (SetStateRequest newOption)

    dropdown <- dropdown (getItem mode) [ getItem normalButton
                                        , getItem starButton
                                        , getItem heartButton
                                        ]

    addPageTile $
      let title = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img   = Nothing -- Just "static/img/remote/example.png"
      in mkTile title img $ list [ listItem (getItem display) (getItem getRandomNumberButton)
                                 , listItem (H.div $ H.text "") (getItem alwaysFailingButton)
                                 , listItem (H.label $ H.text "Mode") (getItem dropdown)
                                 , listItem (H.label $ H.text "Mode") (getItem selectField)
                                 ]

    -- get the current state and set it
    addPageAction $
      runRemote $ do
        liftIO $ putStrLn "[Example] Getting state"
        withResponse GetStateRequest
          doNothing
          (\(GetStateReply state) -> eventStateChangedHandler state)


  buildSchedule _ = return ()
