{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Monad.Logger                 (runStderrLoggingT)
import Database.Persist.MySQL               (ConnectInfo (..), createMySQLPool, defaultConnectInfo)
import Database.Persist.Sql                 (ConnectionPool, runMigration, runSqlPool)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant
import System.Envy
--------------------------------------------------------------------------------
import Api                                  (app)
import Model
import Settings
--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }


mkPool :: Settings -> IO ConnectionPool
mkPool settings =
  let connectInfo = defaultConnectInfo { connectHost     = dbHost     settings
                                       , connectPort     = fromIntegral
                                                         . dbPort   $ settings
                                       , connectUser     = dbUser     settings
                                       , connectPassword = dbPassword settings
                                       , connectDatabase = dbDatabase settings
                                       }
  in runStderrLoggingT (createMySQLPool connectInfo 1)


main :: IO ()
main = do
  msettings <- decodeEnv :: IO (Either String Settings)

  case msettings of
    Left err -> putStrLn err
    Right settings -> do
      print settings

      pool <- mkPool settings

      let config = Config { getPool = pool }

      runSqlPool doMigrations pool
      run 8080 $ logStdoutDev $ corsPolicy $ app config
