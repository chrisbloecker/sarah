{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Control.Monad.Reader
import Database.Persist.Quasi
import Database.Persist.Sql         (SqlPersistM, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
import Data.Text                    (Text)
import Data.Time.Calendar           (Day)
import Data.Time.LocalTime          (TimeOfDay)
--------------------------------------------------------------------------------
import Config
import Types               as Model
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

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
