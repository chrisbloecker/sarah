{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Model
  where
--------------------------------------------------------------------------------
import           Data.Time.Calendar  (Day)
import           Data.Time.LocalTime (TimeOfDay)
import           Import.DeriveJSON
import           Types
--------------------------------------------------------------------------------

data SensorReading = SensorReading { sensorReadingDate   :: Day
                                   , sensorReadingRoom   :: Room
                                   , sensorReadingSensor :: Sensor
                                   , sensorReadingValues :: [(TimeOfDay, Double)]
                                   }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

deriveJSON jsonOptions ''SensorReading
