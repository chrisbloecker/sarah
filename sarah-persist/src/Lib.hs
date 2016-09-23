{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Lib
  ( api
  , server
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Reader       (ReaderT, runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text                  (Text)
import Data.Time.Calendar         (Day)
import Servant
--------------------------------------------------------------------------------
import Model
--------------------------------------------------------------------------------

type AppM = ReaderT Config (EitherT ServantErr IO)

type API = "sensor-readings" :> "date"   :> Capture "date"   Day
                             :> "room"   :> Capture "room"   Room
                             :> "sensor" :> Capture "sensor" Sensor
                             :> Get     '[JSON] [SensorReading]
      :<|> "sensor-readings" :> ReqBody '[JSON] SensorReading
                             :> Put     '[JSON] SensorReading
      :<|> "echo"            :> Get     '[PlainText] Text

--------------------------------------------------------------------------------

api :: Proxy API
api = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither config = Nat $ \x -> runReaderT x config

readerServer :: Config -> Server API
readerServer config = enter (readerToEither config) server

server :: ServerT API AppM
server = getSensorReadings
    :<|> putSensorReading
    :<|> return "echo"

--------------------------------------------------------------------------------

getSensorReadings :: Day -> Room -> Sensor -> AppM [SensorReading]
getSensorReadings day room sensor = undefined

--putSensorReading :: Handler
putSensorReading = undefined
