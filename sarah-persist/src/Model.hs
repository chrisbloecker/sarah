{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Model
  ( module Model
  ) where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ask)
import Database.Persist.Quasi
import Database.Persist.Sql   (runSqlPool)
import Database.Persist.TH
import Data.Text              (Text)
import Data.Time.Calendar     (Day)
import Data.Time.LocalTime    (TimeOfDay)
--------------------------------------------------------------------------------
import Types  as Model
import Config as Model
---------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
SensorReading json
    date   Day
    time   TimeOfDay
    room   Room
    sensor Sensor
    value  Double
    deriving Show
Log json
    date Day
    time TimeOfDay
    text Text
    deriving Show
|]

runDb query = liftIO . runSqlPool query =<< ask getPool
