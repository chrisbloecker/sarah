{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Database
  ( module Sarah.Middleware.Database
  ) where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (MonadReader, asks)
import Data.Binary            (Binary)
import Data.Text              (Text)
import Data.Time.Calendar     (Day)
import Data.Time.LocalTime    (TimeOfDay)
import Database.Persist.Quasi ()
import Database.Persist.Sql   (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Physics
import Sarah.Middleware.Model (Config (getPool))
--------------------------------------------------------------------------------
import Sarah.Middleware.Database.Types as Sarah.Middleware.Database
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
SensorReading json
    date   Day
    time   TimeOfDay
    room   Room
    sensor DeviceAddress
    value  Dimension
    deriving Show
Schedule json
    device DeviceAddress -- ToDo: do we need a DeviceRep here? how do we know what kind of device it is?
    action Command
    timer  Timer
    deriving Show
Log json
    date Day
    time TimeOfDay
    source Text
    text Text
    level LogLevel
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = liftIO . runSqlPool query =<< asks getPool
