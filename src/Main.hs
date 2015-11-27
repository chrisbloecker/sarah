{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

--------------------------------------------------------------------------------
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
--------------------------------------------------------------------------------

type API = "temperature" :> Get '[JSON] Temperature

data Temperature = Celsius Double
  deriving (Eq, Generic)

instance ToJSON Temperature where

--------------------------------------------------------------------------------

temperature :: Temperature
temperature = Celsius 42

server :: Server API
server = return temperature

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
