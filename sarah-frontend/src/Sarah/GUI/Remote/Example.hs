{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (lift, ask)
import Data.Text                      (Text)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), RemoteBuilder, RemoteBuilderEnv (..), sendCommand, embedUI, handleResponse, doNothing)
import Sarah.GUI.Widgets
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand, decodeWrapped)
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

      on click getRandomNumberButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
        handleResponse "[ExampleDevice.getRandomNumberButton.click]" mres doNothing (\(x :: Integer) -> handlerDisplay $ show x)

      on click alwaysFailingButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.AlwaysFailing)
        handleResponse "[ExampleDevice.alwaysFailingButton.click]" mres doNothing (const doNothing :: Text -> IO ())


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

      let eventStateChangedHandler _ = do
            mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetState)
            handleResponse "[ExampleDevice.eventStateChangedHandler]" mres doNothing $ \case
              ExampleDevice.Normal -> handlerStarButton greyButton   >> handlerHeartButton greyButton
              ExampleDevice.Star   -> handlerStarButton yellowButton >> handlerHeartButton greyButton
              ExampleDevice.Heart  -> handlerStarButton greyButton   >> handlerHeartButton redButton

      unregister <- liftIO $ register eventStateChanged eventStateChangedHandler


      on click (getElement minusButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ ExampleDevice.SetState ExampleDevice.Normal)
        handleResponse "[ExampleDevice.minusButton.click]" mres doNothing $ \() -> notifyStateChanged ()

      on click (getElement starButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ ExampleDevice.SetState ExampleDevice.Star)
        handleResponse "[ExampleDevice.starButton.click]" mres doNothing $ \() -> notifyStateChanged ()

      on click (getElement heartButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ ExampleDevice.SetState ExampleDevice.Heart)
        handleResponse "[ExampleDevice.heartButton.click]" mres doNothing $ \() -> notifyStateChanged ()

      liftIO $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetState)
        handleResponse "[ExampleDevice.eventStateChangedHandler]" mres doNothing $ \case
          ExampleDevice.Normal -> handlerStarButton greyButton   >> handlerHeartButton greyButton
          ExampleDevice.Star   -> handlerStarButton yellowButton >> handlerHeartButton greyButton
          ExampleDevice.Heart  -> handlerStarButton greyButton   >> handlerHeartButton redButton

      div #+ [ p # set class_ "text-center"
                 #+ [ element display ]
             , p # set class_ "text-center"
                 #+ map element [getRandomNumberButton, alwaysFailingButton ]
             , p # set class_ "text-center"
                 #+ map element [ minusButton, starButton, heartButton ]
             ]
