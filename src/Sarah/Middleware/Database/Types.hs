{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Database.Types
  ( module Sarah.Middleware.Database.Types
  ) where
--------------------------------------------------------------------------------
import Data.Aeson.TH       (deriveJSON, defaultOptions)
import Data.Binary         (Binary)
import Data.Text           (Text)
import Data.Time.Calendar  (Day (..))
import Data.Time.LocalTime (TimeOfDay (..))
import Database.Persist.TH (derivePersistField)
import GHC.Generics        (Generic)
--------------------------------------------------------------------------------
import Sarah.Middleware.Model as Sarah.Middleware.Database.Types (Command, DeviceAddress, Room)
--------------------------------------------------------------------------------

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
               | TimeOfDay  TimeOfDay
  deriving (Show, Read, Eq, Generic, Binary)

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

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Show, Read, Eq, Generic, Binary)

type DayOfMonth = Int

data TimeInterval = TimeInterval Int
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
