{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (lift, ask, runReaderT)
import Data.Text                      (Text, unpack)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), embedUI, handleResponse, doNothing)
import Sarah.GUI.Widgets
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand, decodeFromText)
import Sarah.Middleware.Device        (ExampleDevice)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Bootstrap.Glyphicon as Glyph
import qualified Sarah.Middleware.Device.Example as ExampleDevice
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      (eventDisplay, handlerDisplay) <- liftIO newEvent
      behaviourDisplay <- stepper "foo" eventDisplay

      display <- reactiveLabel behaviourDisplay

      let buttonClass = buildClass [ btn, btn_sm, btn_default, btn_circle, btn_no_background ]

      getRandomNumberButton <- bootstrapButton buttonClass Glyph.random
      alwaysFailingButton   <- bootstrapButton buttonClass Glyph.flash

      on click getRandomNumberButton $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.GetRandomNumber doNothing (\(x :: Integer) -> handlerDisplay $ show x)
      on click alwaysFailingButton   $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.AlwaysFailing   doNothing (\(_ :: Text) -> doNothing)

      (eventMinusButton, handlerMinusButton) <- liftIO newEvent
      (eventStarButton,  handlerStarButton)  <- liftIO newEvent
      (eventHeartButton, handlerHeartButton) <- liftIO newEvent

      let greyButton   = buildClass [ btn, btn_sm, btn_default, btn_circle, btn_no_background ]
          yellowButton = buildClass [ btn, btn_sm, btn_warning, btn_circle ]
          redButton    = buildClass [ btn, btn_sm, btn_danger,  btn_circle ]

      behaviourMinusButton  <- stepper greyButton eventMinusButton
      behaviourStarButton   <- stepper greyButton eventStarButton
      behaviourHeartButton  <- stepper greyButton eventHeartButton

      minusButton <- reactiveButton behaviourMinusButton (pure $ Style [])
      starButton  <- reactiveButton behaviourStarButton  (pure $ Style [])
      heartButton <- reactiveButton behaviourHeartButton (pure $ Style [])

      element (getElement minusButton) #+ [ span # set class_ (unGlyphicon Glyph.minus) ]
      element (getElement starButton)  #+ [ span # set class_ (unGlyphicon Glyph.star)  ]
      element (getElement heartButton) #+ [ span # set class_ (unGlyphicon Glyph.heart) ]

      let eventStateChangedHandler :: Handler Text
          eventStateChangedHandler encodedState = do
            putStrLn $ "[Example.eventStateChangedHandler] Updating device state: " ++ show encodedState
            case decodeFromText encodedState of
              Nothing -> putStrLn $ "[ExampleDevice.eventStateChangedHandler] Cannot decode state" ++ unpack encodedState
              Just state -> case state of
                ExampleDevice.Normal -> handlerStarButton greyButton   >> handlerHeartButton greyButton
                ExampleDevice.Star   -> handlerStarButton yellowButton >> handlerHeartButton greyButton
                ExampleDevice.Heart  -> handlerStarButton greyButton   >> handlerHeartButton redButton

      unregister <- liftIO $ register eventStateChanged eventStateChangedHandler

      on click (getElement minusButton) $ embedUI $ do
        putStrLn "[Example.minusButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetState ExampleDevice.Normal)

      on click (getElement starButton)  $ embedUI $ do
        putStrLn "[Example.starButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetState ExampleDevice.Star)

      on click (getElement heartButton) $ embedUI $ do
        putStrLn "[Example.heartButton.click]"
        flip runReaderT remoteRunnerEnv $ withoutResponse (ExampleDevice.SetState ExampleDevice.Heart)

      liftIO $ flip runReaderT remoteRunnerEnv $ withResponse ExampleDevice.GetState doNothing eventStateChangedHandler

      div #+ [ p # set class_ "text-center"
                 #+ [ element display ]
             , p # set class_ "text-center"
                 #+ map element [getRandomNumberButton, alwaysFailingButton ]
             , p # set class_ "text-center"
                 #+ map element [ minusButton, starButton, heartButton ]
             ]
