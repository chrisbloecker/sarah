{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Database.Types
  ( module Sarah.Middleware.Database.Types
  ) where
--------------------------------------------------------------------------------
import Data.Aeson.TH                  (deriveJSON, defaultOptions)
import Data.Binary                    (Binary)
import Data.Text                      (Text)
import Data.Time.Calendar             (Day (..), addDays, toGregorian, fromGregorian)
import Data.Time.Calendar.MonthDay    (monthLength)
import Data.Time.Calendar.OrdinalDate (isLeapYear)
import Data.Time.Calendar.WeekDate    (toWeekDate, fromWeekDate)
import Data.Time.Clock                (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime            (TimeOfDay (..), timeOfDayToTime)
import Database.Persist.TH            (derivePersistField)
import GHC.Generics                   (Generic)
--------------------------------------------------------------------------------
import Sarah.Middleware.Model as Sarah.Middleware.Database.Types (Command, DeviceAddress, Room, Query)
--------------------------------------------------------------------------------

-- things that occur at a certain point in time
class HasOccurence timePointDescription where
  nextOccurence :: timePointDescription -> IO UTCTime

data LogLevel = Info
              | Debug
              | Error
  deriving (Show, Read, Eq, Generic, Binary)

data Dimension = Temperature Double -- in degree celsius
               | Humidity    Double -- in percent
  deriving (Show, Read, Generic, Binary)

deriving instance Generic Day
deriving instance Binary  Day
deriving instance Generic TimeOfDay
deriving instance Binary  TimeOfDay

data Timer = Once       Day TimeOfDay
           | Every      TimePoint
           | Repeatedly TimeInterval
  deriving (Show, Read, Eq, Generic, Binary)

data TimePoint = DayOfYear  Month DayOfMonth TimeOfDay
               | DayOfMonth DayOfMonth TimeOfDay
               | DayOfWeek  Weekday TimeOfDay
               | PointOfDay TimeOfDay
  deriving (Show, Read, Eq, Generic, Binary)

instance HasOccurence TimePoint where
  nextOccurence (DayOfYear month dayOfMonth timeOfDay) = do
    now <- getCurrentTime
    let (thisYear, _, _) = toGregorian (utctDay now)
        occurenceThisYear = UTCTime (fromGregorian thisYear (fromMonth month) dayOfMonth) (timeOfDayToTime timeOfDay)

    return $ if now < occurenceThisYear
      then occurenceThisYear
      else UTCTime (fromGregorian (thisYear + 1) (fromMonth month) dayOfMonth) (timeOfDayToTime timeOfDay)


  nextOccurence (DayOfMonth dayOfMonth timeOfDay) = do
    now <- getCurrentTime
    let (thisYear, thisMonth, thisDay) = toGregorian (utctDay now)
        thisMonthLength = monthLength (isLeapYear thisYear) thisMonth

    return $ if dayOfMonth <= thisMonthLength && (thisDay < dayOfMonth || (thisDay == dayOfMonth && utctDayTime now < timeOfDayToTime timeOfDay))
      then UTCTime (fromGregorian thisYear thisMonth dayOfMonth) (timeOfDayToTime timeOfDay)
      else if thisMonth < 12
        then UTCTime (fromGregorian thisYear      (thisMonth + 1) dayOfMonth) (timeOfDayToTime timeOfDay)
        else UTCTime (fromGregorian (thisYear + 1) thisMonth      dayOfMonth) (timeOfDayToTime timeOfDay)


  nextOccurence (DayOfWeek weekday timeOfDay) = do
    now <- getCurrentTime
    let (thisYear, thisWeek, thisWeekday) = toWeekDate (utctDay now)
        dayThisWeek = fromWeekDate thisYear thisWeek (fromWeekday weekday)
        dayNextWeek = addDays 7 dayThisWeek

    return $ if thisWeekday < fromWeekday weekday || (fromWeekday weekday == thisWeekday && utctDayTime now < timeOfDayToTime timeOfDay)
      then UTCTime dayThisWeek (timeOfDayToTime timeOfDay)
      else UTCTime dayNextWeek (timeOfDayToTime timeOfDay)


  nextOccurence (PointOfDay timeOfDay) = do
    now <- getCurrentTime
    let occurenceToday = UTCTime (utctDay now) (timeOfDayToTime timeOfDay)

    return $ if now < occurenceToday
      then occurenceToday
      else UTCTime (addDays 1 (utctDay now)) (timeOfDayToTime timeOfDay)


data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
  deriving (Show, Read, Eq, Generic, Binary)

fromMonth :: Month -> Int
fromMonth January   =  1
fromMonth February  =  2
fromMonth March     =  3
fromMonth April     =  4
fromMonth May       =  5
fromMonth June      =  6
fromMonth July      =  7
fromMonth August    =  8
fromMonth September =  9
fromMonth October   = 10
fromMonth November  = 11
fromMonth December  = 12

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Show, Read, Eq, Generic, Binary)

fromWeekday :: Weekday -> Int
fromWeekday Monday    = 1
fromWeekday Tuesday   = 2
fromWeekday Wednesday = 3
fromWeekday Thursday  = 4
fromWeekday Friday    = 5
fromWeekday Saturday  = 6
fromWeekday Sunday    = 7

type DayOfMonth = Int

newtype TimeInterval = TimeInterval Int
  deriving (Show, Read, Eq, Generic, Binary)


deriveJSON defaultOptions ''Dimension
deriveJSON defaultOptions ''LogLevel
deriveJSON defaultOptions ''Month
deriveJSON defaultOptions ''TimeInterval
deriveJSON defaultOptions ''TimePoint
deriveJSON defaultOptions ''Timer
deriveJSON defaultOptions ''Weekday

derivePersistField "Dimension"
derivePersistField "LogLevel"
derivePersistField "Timer"

--------------------------------------------------------------------------------
-- some other instances that we need for persistence

derivePersistField "Command"
derivePersistField "DeviceAddress"
derivePersistField "Query"
