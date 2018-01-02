{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad                      (forM_, when, void)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Text                          (Text, pack, unpack)
import Data.Time.Calendar                 (Day, fromGregorian)
import Data.Time.LocalTime                (TimeOfDay, midnight)
import Graphics.UI.Material.Types
import Graphics.UI.Material.Icon
import Graphics.UI.Threepenny             (Event, Handler, UI, newEvent, stepper, currentValue, onChanges)
import Graphics.UI.Threepenny.Core        (runFunction, callFunction, ffi)
import Prelude                     hiding (span, div)
import Sarah.GUI.Reactive
import Sarah.Middleware                   (Timer (..), TimePoint (..), TimeInterval (..), Weekday (..))
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

data Dropdown = Dropdown { item   :: H.Html
                         , itemId :: String
                         }

instance IsWidget Dropdown where
  getItem   = item
  getItemId = itemId

dropdown :: MonadIO m => H.Html -> [H.Html] -> m Dropdown
dropdown label items = do
  itemId <- newIdent

  let item = H.div $ do
                 label
                 H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                          H.! A.id (H.toValue itemId) $
                              icon arrow_drop_up
                 H.ul H.! A.class_ "mdl-menu mdl-menu--top-right mdl-js-menu mdl-js-ripple-effect"
                      H.! A.for (H.toValue itemId) $
                          sequence_ items

  return Dropdown{..}

--------------------------------------------------------------------------------

data ReactiveLabel = ReactiveLabel { item      :: H.Html
                                   , itemId    :: String
                                   , event     :: Event     Text
                                   , handler   :: Handler   Text
                                   , behaviour :: Behaviour Text
                                   }

instance IsWidget ReactiveLabel where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveLabel Text where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveLabel :: (MonadIO m, m ~ UI) => Text -> UI ReactiveLabel
reactiveLabel initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialText      <- currentValue behaviour

  let item = H.label H.! A.id (H.toValue itemId) $
                 H.text initialText

  onChanges behaviour $ \newText ->
    runFunction $ ffi "$(%1).text(%2);" ('#':itemId) newText

  return ReactiveLabel{..}

--------------------------------------------------------------------------------

data ReactiveToggle = ReactiveToggle { item      :: H.Html
                                     , itemId    :: String
                                     , event     :: Event Bool
                                     , handler   :: Handler Bool
                                     , behaviour :: Behaviour Bool
                                     }

instance IsWidget ReactiveToggle where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveToggle Bool where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveToggle :: Bool -> UI ReactiveToggle
reactiveToggle initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  labelId          <- newIdent
  itemId           <- newIdent
  initialChecked   <- currentValue behaviour

  let checkbox = H.input H.! A.class_ "mdl-switch__input"
                     H.! A.type_ "checkbox"
                     H.! A.id (H.toValue itemId)
                     H.! A.checked (H.toValue initialChecked)

      item = H.label H.! A.class_ "mdl-switch mdl-js-switch mdl-js-ripple-effect"
                     H.! A.id (H.toValue labelId)
                     H.! A.for (H.toValue itemId) $ do
                         checkbox
                         H.span H.! A.class_ "mdl-switch__label" $ ""

  onChanges behaviour $ \newChecked -> do
    runFunction $ ffi "$(%1).prop('checked', %2);" ('#':itemId) newChecked
    if newChecked
      then runFunction $ ffi "var elem = $(%1); if (elem && elem.hasClass('is-upgraded')) elem[0].MaterialSwitch.on();"  ('#':labelId)
      else runFunction $ ffi "var elem = $(%1); if (elem && elem.hasClass('is-upgraded')) elem[0].MaterialSwitch.off();" ('#':labelId)

  return ReactiveToggle {..}

--------------------------------------------------------------------------------

data ReactiveButton = ReactiveButton { item   :: H.Html
                                     , itemId :: String
                                     }

instance IsWidget ReactiveButton where
  getItem   = item
  getItemId = itemId

reactiveButton :: Text -> Behaviour Text -> UI ReactiveButton
reactiveButton label behaviour = do
  itemId       <- newIdent
  initialClass <- currentValue behaviour

  let item = H.button H.! A.class_ (H.toValue initialClass)
                      H.! A.id (H.toValue itemId) $
                 H.text label

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':itemId) newClass

  return ReactiveButton{..}

--------------------------------------------------------------------------------

data ReactiveCheckbox = ReactiveCheckbox { item   :: H.Html
                                         , itemId :: String
                                         }

instance IsWidget ReactiveCheckbox where
  getItem   = item
  getItemId = itemId

reactiveCheckbox :: Behaviour Bool -> UI ReactiveCheckbox
reactiveCheckbox behaviour = do
  itemId         <- newIdent
  initialChecked <- currentValue behaviour

  let item = H.input H.! A.type_ "checkbox"
                     H.! A.id (H.toValue itemId)
                     H.! A.checked (H.toValue initialChecked)

  onChanges behaviour $ \newChecked ->
    runFunction $ ffi "$(%1).prop('checked',%2);" ('#':itemId) newChecked

  return ReactiveCheckbox{..}

--------------------------------------------------------------------------------

data Cloak = Visible | Hidden

data ReactiveCloak = ReactiveCloak { item      :: H.Html
                                   , itemId    :: String
                                   , event     :: Event     Cloak
                                   , handler   :: Handler   Cloak
                                   , behaviour :: Behaviour Cloak
                                   }

instance IsWidget ReactiveCloak where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveCloak Cloak where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveCloak :: Cloak -> H.Html -> UI ReactiveCloak
reactiveCloak initial child = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialCloak     <- currentValue behaviour

  let toStyle :: Cloak -> Text
      toStyle Visible = ""
      toStyle Hidden = "display: none;"

      item = H.div H.! A.id (H.toValue itemId)
                   H.! A.style (H.toValue $ toStyle initialCloak) $
                 child

  onChanges behaviour $ \cloak ->
    runFunction $ ffi "document.getElementById(%1).style = %2;" itemId (toStyle cloak)

  return ReactiveCloak{..}

--------------------------------------------------------------------------------

data DatePicker = DatePicker { item   :: H.Html
                             , itemId :: String
                             , input  :: UI Day
                             }

instance IsWidget DatePicker where
  getItem   = item
  getItemId = itemId

instance HasInput DatePicker Day where
  getInput = input

datePicker :: UI DatePicker
datePicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.type_ "date"
                         H.! A.id (H.toValue itemId)

      input = do
        liftIO $ putStrLn "[datePicker.getInput]"
        theInput <- getValue itemId
        liftIO . putStrLn $ "The input was: " ++ theInput
        return (fromGregorian 1986 12 22)

  return DatePicker{..}

--------------------------------------------------------------------------------

data TimePicker = TimePicker { item   :: H.Html
                             , itemId :: String
                             , input  :: UI TimeOfDay
                             }

instance IsWidget TimePicker where
  getItem   = item
  getItemId = itemId

instance HasInput TimePicker TimeOfDay where
  getInput = input

timePicker :: UI TimePicker
timePicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.type_ "time"
                         H.! A.id (H.toValue itemId)

      input = do
        liftIO $ putStrLn "[timePicker.getInput]"
        theInput <- getValue itemId
        liftIO . putStrLn $ "The input was: " ++ theInput
        return midnight

  return TimePicker{..}

--------------------------------------------------------------------------------

data DateTimePicker = DateTimePicker { item   :: H.Html
                                     , itemId :: String
                                     , input  :: UI (Day, TimeOfDay)
                                     }

instance IsWidget DateTimePicker where
  getItem   = item
  getItemId = itemId

instance HasInput DateTimePicker (Day, TimeOfDay) where
  getInput = input

dateTimePicker :: UI DateTimePicker
dateTimePicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.type_ "datetime-local"
                         H.! A.id (H.toValue itemId)

      input = do
        liftIO $ putStrLn "[dateTimePicker.getInput]"
        theInput <- getValue itemId
        liftIO . putStrLn $ "The input was: " ++ theInput
        return (fromGregorian 1986 12 22, midnight)

  return DateTimePicker{..}

--------------------------------------------------------------------------------

data ReactiveOption option = ReactiveOption { item   :: H.Html
                                            , itemId :: String
                                            , input  :: UI option
                                            }

instance HasSelection option => IsWidget (ReactiveOption option) where
  getItem   = item
  getItemId = itemId

instance HasSelection option => HasInput (ReactiveOption option) option where
  getInput = input

reactiveOption :: HasSelection option => option -> UI (ReactiveOption option)
reactiveOption option = do
  itemId <- newIdent

  let item = H.option H.! A.value (H.toValue . toSelectionLabel $ option) $
                 H.text (toSelectionLabel option)

      input = return option

  return ReactiveOption{..}

--------------------------------------------------------------------------------

data ReactiveSelectField option = ReactiveSelectField { item      :: H.Html
                                                      , itemId    :: String
                                                      , event     :: Event     option
                                                      , handler   :: Handler   option
                                                      , behaviour :: Behaviour option
                                                      }

instance HasSelection option => IsWidget (ReactiveSelectField option) where
  getItem   = item
  getItemId = itemId

instance HasSelection option => IsReactive (ReactiveSelectField option) option where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveSelectField :: HasSelection option
                    => [ReactiveOption option] -> option -> UI (ReactiveSelectField option)
reactiveSelectField options initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialSelected  <- currentValue behaviour

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ --do
                 H.select H.! A.class_ "mdl-textfield__input"
                          H.! A.id (H.toValue itemId) $
                     forM_ options getItem
--                 H.label H.! A.class_ "mdl-textfield__label"
--                         H.! A.for (H.toValue itemId) $
--                     H.text (toSelectionLabel initialSelected)
{-
  forM_ options $ \option ->
    onElementIDClick (getItemId option) $ do

       theOption <- getInput option
       liftIO $ handler theOption
-}
  onChanges behaviour $ \newSelection ->
    runFunction $ ffi "document.getElementById(%1).value = %2; console.log(%2);" itemId (toSelectionLabel newSelection)

  return ReactiveSelectField{..}

--------------------------------------------------------------------------------

data ReactiveListItem = ReactiveListItem { item      :: H.Html
                                         , itemId    :: String
                                         , event     :: Event     Text
                                         , handler   :: Handler   Text
                                         , behaviour :: Behaviour Text
                                         }

instance IsWidget ReactiveListItem where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveListItem Text where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveListItem :: Text -> Text -> UI ReactiveListItem
reactiveListItem label initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialClass     <- currentValue behaviour

  let item = H.li H.! A.class_ "mdl-list__item" $
                 H.div H.! A.id (H.toValue itemId)
                       H.! A.class_ (H.toValue initialClass) $
                     H.text label

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':itemId) newClass

  return ReactiveListItem{..}

--------------------------------------------------------------------------------

data TimePointInput = TimePointInput { item   :: H.Html
                                     , itemId :: String
                                     , input  :: UI TimePoint
                                     }

instance IsWidget TimePointInput where
  getItem   = item
  getItemId = itemId

instance HasInput TimePointInput TimePoint where
  getInput = input

timePointInput :: UI TimePointInput
timePointInput = do
  itemId <- newIdent

  let item = H.text "Time Point"

      input = do
        liftIO $ putStrLn "[timePointInput.getInput]"
        theInput <- getValue itemId
        liftIO . putStrLn $ "The input was: " ++ theInput
        return (DayOfWeek Monday)

  return TimePointInput{..}

--------------------------------------------------------------------------------

data TimeIntervalInput = TimeIntervalInput { item   :: H.Html
                                           , itemId :: String
                                           , input  :: UI TimeInterval
                                           }

instance IsWidget TimeIntervalInput where
  getItem   = item
  getItemId = itemId

instance HasInput TimeIntervalInput TimeInterval where
  getInput = input

timeIntervalInput :: UI TimeIntervalInput
timeIntervalInput = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $ do
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.id (H.toValue itemId)
                         H.! A.type_ "text"
                         H.! A.pattern "-?[1-9][0-9]*"
                 H.label H.! A.class_ "mdl-textfield__label"
                         H.! A.for (H.toValue itemId) $
                     H.text "Time interval"
                 H.span  H.! A.class_ "mdl-textfield__error" $
                     H.text "Input is not a number!"

      input = do
        liftIO $ putStrLn "[TimeIntervalInput.getInput]"
        theInput <- getValue itemId
        liftIO . putStrLn $ "The input was: " ++ theInput
        return (TimeInterval 42)

  return TimeIntervalInput{..}

--------------------------------------------------------------------------------

data TimerInput = TimerInput { item    :: H.Html
                             , itemId  :: String
                             , actions :: [UI ()]
                             , input   :: UI Timer
                             }

instance IsWidget TimerInput where
  getItem   = item
  getItemId = itemId

instance HasInput TimerInput Timer where
  getInput = input

instance HasPageActions TimerInput where
  getPageActions = actions

data TimerInputOptions = TimerInputOnce
                       | TimerInputEvery
                       | TimerInputRepeatedly
  deriving (Show)

instance HasSelection TimerInputOptions where
  toSelectionLabel TimerInputOnce       = "Once"
  toSelectionLabel TimerInputEvery      = "Every"
  toSelectionLabel TimerInputRepeatedly = "Repeatedly"

  fromSelectionLabel "Once"       = Right TimerInputOnce
  fromSelectionLabel "Every"      = Right TimerInputEvery
  fromSelectionLabel "Repeatedly" = Right TimerInputRepeatedly
  fromSelectionLabel t            = unexpectedSelectionLabel t

timerInput :: UI TimerInput
timerInput = do
  --itemId <- newIdent

  optionOnce       <- reactiveOption TimerInputOnce
  optionEvery      <- reactiveOption TimerInputEvery
  optionRepeatedly <- reactiveOption TimerInputRepeatedly
  timerOptions     <- reactiveSelectField [optionOnce, optionEvery, optionRepeatedly] TimerInputOnce

  let itemId = getItemId timerOptions

  onceInput       <- dateTimePicker
  everyInput      <- timePointInput
  repeatedlyInput <- timeIntervalInput

  onceCloaked       <- reactiveCloak Visible (getItem onceInput)
  everyCloaked      <- reactiveCloak Hidden  (getItem everyInput)
  repeatedlyCloaked <- reactiveCloak Hidden  (getItem repeatedlyInput)

  let item = H.div $
                 H.ul H.! A.class_ "mdl-list" $ sequence_
                    [ getItem timerOptions
                    , getItem onceCloaked
                    , getItem everyCloaked
                    , getItem repeatedlyCloaked
                    ]

      onChangeTimer = onElementIDChange (getItemId timerOptions) $ fmap liftIO $ \case
                        TimerInputOnce       -> sequence_ [getHandler timerOptions TimerInputOnce,       getHandler onceCloaked Visible, getHandler everyCloaked Hidden,  getHandler repeatedlyCloaked Hidden ]
                        TimerInputEvery      -> sequence_ [getHandler timerOptions TimerInputEvery,      getHandler onceCloaked Hidden,  getHandler everyCloaked Visible, getHandler repeatedlyCloaked Hidden ]
                        TimerInputRepeatedly -> sequence_ [getHandler timerOptions TimerInputRepeatedly, getHandler onceCloaked Hidden,  getHandler everyCloaked Hidden,  getHandler repeatedlyCloaked Visible]

      actions = [onChangeTimer]

      input = do
        selection <- currentValue (getBehaviour timerOptions)
        case selection of
          TimerInputOnce       -> uncurry Once <$> getInput onceInput
          TimerInputEvery      -> Every        <$> getInput everyInput
          TimerInputRepeatedly -> Repeatedly   <$> getInput repeatedlyInput

  return TimerInput{..}
