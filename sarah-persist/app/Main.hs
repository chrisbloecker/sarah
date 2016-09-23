module Main
  where
--------------------------------------------------------------------------------
import Database.Persist.Sql                 (runMigration, runSqlPool)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.Envy
--------------------------------------------------------------------------------
import Api                                  (app)
import Api.Sensor                           (generateJavaScript)
import Config
import Model
import Settings
--------------------------------------------------------------------------------

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
      run 8080 $ logStdoutDev $ app config
