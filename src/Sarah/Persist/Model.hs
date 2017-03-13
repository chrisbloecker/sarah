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
module Sarah.Persist.Model
  ( module Sarah.Persist.Model
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Except   (MonadError, ExceptT)
import Control.Monad.Reader   (MonadIO, MonadReader, ReaderT, asks, liftIO)
import Database.Persist.Quasi
import Database.Persist.Sql   (ConnectionPool, SqlPersistM, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
import Data.Text              (Text)
import Data.Time.Calendar     (Day)
import Data.Time.LocalTime    (TimeOfDay)
import Servant                (ServantErr)
--------------------------------------------------------------------------------
import Sarah.Persist.Types as Sarah.Persist.Model
--------------------------------------------------------------------------------

newtype PersistApp a = PersistApp { runPersistApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

data Config = Config { getPool :: ConnectionPool }

--------------------------------------------------------------------------------

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
    source Text
    text Text
    level LogLevel
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
