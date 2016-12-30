{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Api.Log
  ( LogApi
  , logServer
  ) where
--------------------------------------------------------------------------------
import Control.Monad
import Database.Persist
import Sarah.Persist.Model
import Servant
--------------------------------------------------------------------------------

type LogApi = "log" :> Get     '[JSON] [Log]
         :<|> "log" :> ReqBody '[JSON] Log
                    :> Put     '[JSON] ()

logServer :: ServerT LogApi PersistApp
logServer = getLog
       :<|> putLog

getLog :: PersistApp [Log]
getLog = fmap (map entityVal) $ runDb $ selectList [] []

putLog :: Log -> PersistApp ()
putLog log = void $ runDb (insert log)
