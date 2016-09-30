{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Database.Persist.Sql                 (runMigration, runSqlPool)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant
import System.Envy
--------------------------------------------------------------------------------
import Api                                  (app)
import Api.Sensor                           (generateJavaScript)
import Config
import Model
import Settings
--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

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
      generateJavaScript
      run 8080 $ logStdoutDev $ corsPolicy $ app config
