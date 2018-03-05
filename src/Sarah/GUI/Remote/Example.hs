{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Control.Monad                   (void)
import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Reader            (lift, ask)
import Data.Foldable                   (traverse_)
import Data.Text                       (Text, append, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny          (UI, Handler, register, runFunction, ffi)
import Prelude                  hiding (unwords)
import Sarah.GUI.Reactive
import Sarah.GUI.Model
import Sarah.GUI.Websocket
import Sarah.Middleware
import Sarah.Middleware.Device.Example
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

deriving instance Show (DeviceState   ExampleDevice)
deriving instance Show (DeviceRequest ExampleDevice)

instance HasSelection (DeviceState ExampleDevice) where
  toSelectionLabel Normal = "Normal"
  toSelectionLabel Star   = "Star"
  toSelectionLabel Heart  = "Heart"

  fromSelectionLabel "Normal" = Right Normal
  fromSelectionLabel "Star"   = Right Star
  fromSelectionLabel "Heart"  = Right Heart
  fromSelectionLabel t        = unexpectedSelectionLabel t

instance HasSelection (DeviceRequest ExampleDevice) where
  toSelectionLabel RandomNumberRequest         = "Random number request"
  toSelectionLabel (SetStateRequest Normal)    = "Set state to normal"
  toSelectionLabel (SetStateRequest Star)      = "Set state to star"
  toSelectionLabel (SetStateRequest Heart)     = "Set state to heart"
  toSelectionLabel GetStateRequest             = "Get state"
  toSelectionLabel AlwaysFailingRequest        = "Always failing request"

  fromSelectionLabel "Random number request"  = Right RandomNumberRequest
  fromSelectionLabel "Set state to normal"    = Right (SetStateRequest Normal)
  fromSelectionLabel "Set state to star"      = Right (SetStateRequest Star)
  fromSelectionLabel "Set state to heart"     = Right (SetStateRequest Heart)
  fromSelectionLabel "Get state"              = Right GetStateRequest
  fromSelectionLabel "Always failing request" = Right AlwaysFailingRequest
  fromSelectionLabel t                        = unexpectedSelectionLabel t

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

    selectField  <- lift $ reactiveSelectField [normalOption, starOption, heartOption] Normal

    let eventStateChangedHandler :: Handler (DeviceState ExampleDevice)
        eventStateChangedHandler = \case
          Normal -> sequence_ [getHandler normalButton accented, getHandler starButton grey,     getHandler heartButton grey,     getHandler mode ("Normal" :: Text), getHandler selectField Normal]
          Star   -> sequence_ [getHandler normalButton grey,     getHandler starButton accented, getHandler heartButton grey,     getHandler mode ("Star"   :: Text), getHandler selectField Star  ]
          Heart  -> sequence_ [getHandler normalButton grey,     getHandler starButton grey,     getHandler heartButton accented, getHandler mode ("Heart"  :: Text), getHandler selectField Heart ]

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
      in mkTileSmall title img $ list [ listItem (getItem display) (getItem getRandomNumberButton)
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


  buildSchedule _ = do
    ScheduleBuilderEnv{..} <- ask
    schedule               <- getSchedule

    optionNormal   <- lift $ reactiveOption (SetStateRequest Normal)
    optionStar     <- lift $ reactiveOption (SetStateRequest Star)
    optionHeart    <- lift $ reactiveOption (SetStateRequest Heart)
    scheduleAction <- lift $ reactiveSelectField [optionNormal, optionStar, optionHeart] (SetStateRequest Normal)
    scheduleTimer  <- lift timerInput

    addItemButton   <- button Nothing (Just "Add")
    addItemDialogue <- dialogue "Add schedule" $ list [ getItem scheduleAction
                                                      , getItem scheduleTimer
                                                      ]

    traverse_ addPageAction (getPageActions scheduleTimer)

    addPageAction $
      onElementIDChange (getItemId scheduleTimer) $ \(newSelection :: TimerInputOptions) -> do
        runFunction $ ffi "console.log('New selection: %1')" (show newSelection)
        liftIO . putStrLn $ "New option is " ++ show newSelection

    -- display the dialogue to add a schedule item
    addPageAction $
      onElementIDClick (getItemId addItemButton) $
        showDialogue (getItemId addItemDialogue)

    -- submitting the new schedule item through the dialogue
    addPageAction $
      onElementIDClick (getSubmitButtonId addItemDialogue) $ do
        mAction <- getInput scheduleAction :: UI (Maybe (DeviceRequest ExampleDevice))
        mTimer  <- getInput scheduleTimer  :: UI (Maybe Timer)

        case (,) <$> mAction <*> mTimer of
          Nothing              -> return ()
          Just (action, timer) -> do
            let request = CreateScheduleRequest (Schedule deviceAddress (mkQuery deviceAddress action) timer)
            void $ toMaster middleware request

    addPageAction $
      onElementIDClick (getDismissButtonId addItemDialogue) $
        hideDialogue (getItemId addItemDialogue)

    addPageDialogue $
      getItem addItemDialogue

    addPageTile $
      let title         = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img           = Nothing
          scheduleItems = map (\(_, Schedule{..}) -> case getCommand . queryCommand $ scheduleAction of
                                                       Left err                                       -> mempty
                                                       Right (command :: DeviceRequest ExampleDevice) -> listItem (H.text . pack . show $ scheduleTimer)
                                                                                                                  (H.text . pack . show $ command)
                              ) schedule
      in mkTileSmall title img (list $ scheduleItems ++ [getItem addItemButton])
