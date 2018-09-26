{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Combinators
import Control.Applicative                (liftA2, liftA3)
import Control.Arrow                      ((&&&))
import Control.Monad                      ((>=>), forM_, when, void)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Text                          (Text, pack, unpack)
import Data.Time.Calendar                 (Day, addDays)
import Data.Time.Clock                    (UTCTime (..))
import Data.Time.Format                   (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime                (TimeOfDay, getCurrentTimeZone, timeToTimeOfDay, timeOfDayToTime, localToUTCTimeOfDay)
import Graphics.UI.Material.Types
import Graphics.UI.Material.Icon
import Graphics.UI.Threepenny             (Event, Handler, UI, newEvent, stepper, currentValue, onChanges)
import Graphics.UI.Threepenny.Core        (runFunction, callFunction, ffi)
import Prelude                     hiding (span, div)
import Sarah.GUI.Reactive
import Sarah.Middleware                   (Timer (..), TimePoint (..), TimeInterval (..), Month (..), DayOfMonth, Weekday (..))
import Text.Read                          (readMaybe)
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
    runFunction $ ffi "var elem = document.getElementById(%1); if (elem) { elem.style = %2; } else { console.log('Could not find element to cloak: %1.'); }" itemId (toStyle cloak)

  return ReactiveCloak{..}

--------------------------------------------------------------------------------

data DatePicker = DatePicker { item   :: H.Html
                             , itemId :: String
                             , input  :: UI (Maybe Day)
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
        fmap utctDay <$> parseTimeM True defaultTimeLocale "%F" <$> getValue itemId

  return DatePicker{..}

--------------------------------------------------------------------------------

data TimePicker = TimePicker { item    :: H.Html
                             , itemId  :: String
                             , actions :: [UI ()]
                             , input   :: UI (Maybe TimeOfDay)
                             }

instance IsWidget TimePicker where
  getItem   = item
  getItemId = itemId

instance HasPageActions TimePicker where
  getPageActions = actions

instance HasInput TimePicker TimeOfDay where
  getInput = input

-- A time picker that returns a time of day in UTC.
timePicker :: UI TimePicker
timePicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.type_ "time"
                         H.! A.id (H.toValue itemId)

      actions = []

      input = do
        timeZone  <- liftIO getCurrentTimeZone
        localTime <- parseTimeM True defaultTimeLocale "%R" <$> getValue itemId
        return . snd . localToUTCTimeOfDay timeZone <$> localTime

  return TimePicker{..}

--------------------------------------------------------------------------------

data DateTimePicker = DateTimePicker { item    :: H.Html
                                     , itemId  :: String
                                     , actions :: [UI ()]
                                     , input   :: UI (Maybe (Day, TimeOfDay))
                                     }

instance IsWidget DateTimePicker where
  getItem   = item
  getItemId = itemId

instance HasInput DateTimePicker (Day, TimeOfDay) where
  getInput = input

instance HasPageActions DateTimePicker where
  getPageActions = actions

dateTimePicker :: UI DateTimePicker
dateTimePicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.type_ "datetime-local"
                         H.! A.id (H.toValue itemId)

      input = do
        mDateTime <- parseTimeM True defaultTimeLocale "%FT%R" <$> getValue itemId
        timeZone  <- liftIO getCurrentTimeZone
        return $ case mDateTime of
          Nothing -> Nothing
          Just dateTime -> do
            let (dayOffset, utcTimeOfDay) = localToUTCTimeOfDay timeZone . timeToTimeOfDay . utctDayTime $ dateTime
            Just (addDays dayOffset . utctDay $ dateTime, utcTimeOfDay)

      actions = []

  return DateTimePicker{..}

--------------------------------------------------------------------------------

data ReactiveOption option = ReactiveOption { item   :: H.Html
                                            , itemId :: String
                                            , input  :: UI (Maybe option)
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

      input = return (Just option)

  return ReactiveOption{..}

--------------------------------------------------------------------------------

data ReactiveSelectField option = ReactiveSelectField { item      :: H.Html
                                                      , itemId    :: String
                                                      , event     :: Event     option
                                                      , handler   :: Handler   option
                                                      , behaviour :: Behaviour option
                                                      , input     :: UI (Maybe option)
                                                      }

instance HasSelection option => IsWidget (ReactiveSelectField option) where
  getItem   = item
  getItemId = itemId

instance HasSelection option => IsReactive (ReactiveSelectField option) option where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

instance HasSelection option => HasInput (ReactiveSelectField option) option where
  getInput = input

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

  onChanges behaviour $ \newSelection ->
    runFunction $ ffi "document.getElementById(%1).value = %2; console.log(%2);" itemId (toSelectionLabel newSelection)

  let input = Just <$> currentValue behaviour

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

data MonthInput = MonthInput { item    :: H.Html
                             , itemId  :: String
                             , actions :: [UI ()]
                             , input   :: UI (Maybe Month)
                             }

instance IsWidget MonthInput where
  getItem   = item
  getItemId = itemId

instance HasInput MonthInput Month where
  getInput = input

instance HasPageActions MonthInput where
  getPageActions = actions

instance HasSelection Month where
  toSelectionLabel January   = "January"
  toSelectionLabel February  = "February"
  toSelectionLabel March     = "March"
  toSelectionLabel April     = "April"
  toSelectionLabel May       = "May"
  toSelectionLabel June      = "June"
  toSelectionLabel July      = "July"
  toSelectionLabel August    = "August"
  toSelectionLabel September = "September"
  toSelectionLabel October   = "October"
  toSelectionLabel November  = "November"
  toSelectionLabel December  = "December"

  fromSelectionLabel "January"   = Right January
  fromSelectionLabel "February"  = Right February
  fromSelectionLabel "March"     = Right March
  fromSelectionLabel "April"     = Right April
  fromSelectionLabel "May"       = Right May
  fromSelectionLabel "June"      = Right June
  fromSelectionLabel "July"      = Right July
  fromSelectionLabel "August"    = Right August
  fromSelectionLabel "September" = Right September
  fromSelectionLabel "October"   = Right October
  fromSelectionLabel "November"  = Right November
  fromSelectionLabel "December"  = Right December
  fromSelectionLabel t           = unexpectedSelectionLabel t

monthPicker :: UI MonthInput
monthPicker = do
  january      <- reactiveOption January
  february     <- reactiveOption February
  march        <- reactiveOption March
  april        <- reactiveOption April
  may          <- reactiveOption May
  june         <- reactiveOption June
  july         <- reactiveOption July
  august       <- reactiveOption August
  september    <- reactiveOption September
  october      <- reactiveOption October
  november     <- reactiveOption November
  december     <- reactiveOption December
  monthOptions <- reactiveSelectField [january, february, march, april, may, june, july, august, september, october, november, december] January

  let item   = getItem   monthOptions
      itemId = getItemId monthOptions

      onChangeMonth = onElementIDChange (getItemId monthOptions) $ fmap liftIO $ \(month :: Month) ->
                        getHandler monthOptions month

      actions = [onChangeMonth]

      input = Just <$> currentValue (getBehaviour monthOptions)

  return MonthInput{..}

--------------------------------------------------------------------------------

data DayOfMonthInput = DayOfMonthInput { item    :: H.Html
                                       , itemId  :: String
                                       , actions :: [UI ()]
                                       , input   :: UI (Maybe DayOfMonth)
                                       }

instance IsWidget DayOfMonthInput where
  getItem   = item
  getItemId = itemId

instance HasPageActions DayOfMonthInput where
  getPageActions = actions

instance HasInput DayOfMonthInput DayOfMonth where
  getInput = input

dayOfMonthPicker :: UI DayOfMonthInput
dayOfMonthPicker = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $ do
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.id (H.toValue itemId)
                         H.! A.type_ "text"
                         H.! A.pattern "[1-9]|[12][0-9]|30|31"
                 H.label H.! A.class_ "mdl-textfield__label"
                         H.! A.for (H.toValue itemId) $
                     H.text "Day of month"
                 H.span  H.! A.class_ "mdl-textfield__error"$
                     H.text "Input is not a valid number!"

      actions = []

      input = over (getValue itemId) readMaybe

  return DayOfMonthInput{..}

--------------------------------------------------------------------------------

data WeekdayInput = WeekdayInput { item    :: H.Html
                                 , itemId  :: String
                                 , actions :: [UI ()]
                                 , input   :: UI (Maybe Weekday)
                                 }

instance IsWidget WeekdayInput where
  getItem   = item
  getItemId = itemId

instance HasInput WeekdayInput Weekday where
  getInput = input

instance HasPageActions WeekdayInput where
  getPageActions = actions

instance HasSelection Weekday where
  toSelectionLabel Monday    = "Monday"
  toSelectionLabel Tuesday   = "Tuesday"
  toSelectionLabel Wednesday = "Wednesday"
  toSelectionLabel Thursday  = "Thursday"
  toSelectionLabel Friday    = "Friday"
  toSelectionLabel Saturday  = "Saturday"
  toSelectionLabel Sunday    = "Sunday"

  fromSelectionLabel "Monday"    = Right Monday
  fromSelectionLabel "Tuesday"   = Right Tuesday
  fromSelectionLabel "Wednesday" = Right Wednesday
  fromSelectionLabel "Thursday"  = Right Thursday
  fromSelectionLabel "Friday"    = Right Friday
  fromSelectionLabel "Saturday"  = Right Saturday
  fromSelectionLabel "Sunday"    = Right Sunday
  fromSelectionLabel t           = unexpectedSelectionLabel t

weekdayPicker :: UI WeekdayInput
weekdayPicker = do
  monday         <- reactiveOption Monday
  tuesday        <- reactiveOption Tuesday
  wednesday      <- reactiveOption Wednesday
  thursday       <- reactiveOption Thursday
  friday         <- reactiveOption Friday
  saturday       <- reactiveOption Saturday
  sunday         <- reactiveOption Sunday
  weekdayOptions <- reactiveSelectField [monday, tuesday, wednesday, thursday, friday, saturday, sunday] Monday

  let item   = getItem   weekdayOptions
      itemId = getItemId weekdayOptions

      onChangeWeekday = onElementIDChange (getItemId weekdayOptions) $ fmap liftIO $ \(weekday :: Weekday) ->
                          getHandler weekdayOptions weekday

      actions = [onChangeWeekday]

      input = Just <$> currentValue (getBehaviour weekdayOptions)

  return WeekdayInput{..}

--------------------------------------------------------------------------------

data TimePointInput = TimePointInput { item    :: H.Html
                                     , itemId  :: String
                                     , actions :: [UI ()]
                                     , input   :: UI (Maybe TimePoint)
                                     }

instance IsWidget TimePointInput where
  getItem   = item
  getItemId = itemId

instance HasInput TimePointInput TimePoint where
  getInput = input

instance HasPageActions TimePointInput where
  getPageActions = actions

data TimePointInputOptions = TimePointInputDayOfYear
                           | TimePointInputDayOfMonth
                           | TimePointInputDayOfWeek
                           | TimePointInputPointOfDay

instance HasSelection TimePointInputOptions where
  toSelectionLabel TimePointInputDayOfYear  = "Day of year"
  toSelectionLabel TimePointInputDayOfMonth = "Day of month"
  toSelectionLabel TimePointInputDayOfWeek  = "Day of week"
  toSelectionLabel TimePointInputPointOfDay  = "Time of day"

  fromSelectionLabel "Day of year"  = Right TimePointInputDayOfYear
  fromSelectionLabel "Day of month" = Right TimePointInputDayOfMonth
  fromSelectionLabel "Day of week"  = Right TimePointInputDayOfWeek
  fromSelectionLabel "Time of day"  = Right TimePointInputPointOfDay
  fromSelectionLabel t              = unexpectedSelectionLabel t

timePointInput :: UI TimePointInput
timePointInput = do
  optionDayOfYear  <- reactiveOption TimePointInputDayOfYear
  optionDayOfMonth <- reactiveOption TimePointInputDayOfMonth
  optionDayOfWeek  <- reactiveOption TimePointInputDayOfWeek
  optionPointOfDay <- reactiveOption TimePointInputPointOfDay
  timePointOptions <- reactiveSelectField [optionDayOfYear, optionDayOfMonth, optionDayOfWeek, optionPointOfDay] TimePointInputDayOfYear

  monthInput      <- monthPicker
  dayOfMonthInput <- dayOfMonthPicker
  weekdayInput    <- weekdayPicker
  pointOfDayInput <- timePicker

  monthCloaked      <- reactiveCloak Visible (getItem monthInput)
  dayOfMonthCloaked <- reactiveCloak Visible (getItem dayOfMonthInput)
  weekdayCloaked    <- reactiveCloak Hidden  (getItem weekdayInput)

  let itemId = getItemId timePointOptions

      item = H.div $
                 H.ul H.! A.class_ "mdl-list" $ sequence_
                     [ getItem timePointOptions
                     , getItem monthCloaked
                     , getItem dayOfMonthCloaked
                     , getItem weekdayCloaked
                     , getItem pointOfDayInput
                     ]

      onChangeTimePoint = onElementIDChange (getItemId timePointOptions) $ fmap liftIO $ \case
                              TimePointInputDayOfYear  -> sequence_ [getHandler timePointOptions TimePointInputDayOfYear,  getHandler monthCloaked Visible, getHandler dayOfMonthCloaked Visible, getHandler weekdayCloaked Hidden ]
                              TimePointInputDayOfMonth -> sequence_ [getHandler timePointOptions TimePointInputDayOfMonth, getHandler monthCloaked Hidden,  getHandler dayOfMonthCloaked Visible, getHandler weekdayCloaked Hidden ]
                              TimePointInputDayOfWeek  -> sequence_ [getHandler timePointOptions TimePointInputDayOfWeek,  getHandler monthCloaked Hidden,  getHandler dayOfMonthCloaked Hidden,  getHandler weekdayCloaked Visible]
                              TimePointInputPointOfDay -> sequence_ [getHandler timePointOptions TimePointInputPointOfDay, getHandler monthCloaked Hidden,  getHandler dayOfMonthCloaked Hidden,  getHandler weekdayCloaked Hidden ]

      actions = [onChangeTimePoint]
             ++ getPageActions monthInput
             ++ getPageActions dayOfMonthInput
             ++ getPageActions weekdayInput
             ++ getPageActions pointOfDayInput

      input = currentValue (getBehaviour timePointOptions) >>= \case
                TimePointInputDayOfYear  -> liftA3 DayOfYear  <$> getInput monthInput
                                                              <*> getInput dayOfMonthInput
                                                              <*> getInput pointOfDayInput
                TimePointInputDayOfMonth -> liftA2 DayOfMonth <$> getInput dayOfMonthInput
                                                              <*> getInput pointOfDayInput
                TimePointInputDayOfWeek  -> liftA2 DayOfWeek  <$> getInput weekdayInput
                                                              <*> getInput pointOfDayInput
                TimePointInputPointOfDay -> fmap PointOfDay   <$> getInput pointOfDayInput

  return TimePointInput{..}

--------------------------------------------------------------------------------

data TimeIntervalInput = TimeIntervalInput { item    :: H.Html
                                           , itemId  :: String
                                           , actions :: [UI ()]
                                           , input   :: UI (Maybe TimeInterval)
                                           }

instance IsWidget TimeIntervalInput where
  getItem   = item
  getItemId = itemId

instance HasInput TimeIntervalInput TimeInterval where
  getInput = input

instance HasPageActions TimeIntervalInput where
  getPageActions = actions

timeIntervalInput :: UI TimeIntervalInput
timeIntervalInput = do
  itemId <- newIdent

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield" $ do
                 H.input H.! A.class_ "mdl-textfield__input"
                         H.! A.id (H.toValue itemId)
                         H.! A.type_ "text"
                         H.! A.pattern "[1-9][0-9]*"
                 H.label H.! A.class_ "mdl-textfield__label"
                         H.! A.for (H.toValue itemId) $
                     H.text "Time interval in seconds"
                 H.span  H.! A.class_ "mdl-textfield__error" $
                     H.text "Input is not a number!"

      input = do
        liftIO $ putStrLn "[TimeIntervalInput.getInput]"
        over (getValue itemId) ((readMaybe :: String -> Maybe Int) >=> Just . TimeInterval)

      actions = []

  return TimeIntervalInput{..}

--------------------------------------------------------------------------------

data TimerInput = TimerInput { item    :: H.Html
                             , itemId  :: String
                             , actions :: [UI ()]
                             , input   :: UI (Maybe Timer)
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
  optionOnce        <- reactiveOption TimerInputOnce
  optionEvery       <- reactiveOption TimerInputEvery
  optionRepeatedly  <- reactiveOption TimerInputRepeatedly
  timerOptions      <- reactiveSelectField [optionOnce, optionEvery, optionRepeatedly] TimerInputOnce

  onceInput         <- dateTimePicker
  everyInput        <- timePointInput
  repeatedlyInput   <- timeIntervalInput

  onceCloaked       <- reactiveCloak Visible (getItem onceInput)
  everyCloaked      <- reactiveCloak Hidden  (getItem everyInput)
  repeatedlyCloaked <- reactiveCloak Hidden  (getItem repeatedlyInput)

  let itemId = getItemId timerOptions

      item = H.div $
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
             ++ getPageActions onceInput
             ++ getPageActions everyInput
             ++ getPageActions repeatedlyInput

      input = do
        selection <- currentValue (getBehaviour timerOptions)
        case selection of
          TimerInputOnce       -> fmap (uncurry Once) <$> getInput onceInput
          TimerInputEvery      -> fmap Every          <$> getInput everyInput
          TimerInputRepeatedly -> fmap Repeatedly     <$> getInput repeatedlyInput

  return TimerInput{..}
