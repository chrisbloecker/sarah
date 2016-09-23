module Main
  where
--------------------------------------------------------------------------------
import Control.Monad.Logger     (runStderrLoggingT)
import Database.Persist.Sql     (runMigration, runSqlPool)
import Database.Persist.MySQL   (ConnectInfo (..), createMySQLPool, defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Envy
--------------------------------------------------------------------------------
import Lib
import Model                    (migrateAll)
import Settings
--------------------------------------------------------------------------------

main :: IO ()
main = do
  msettings <- decodeEnv :: IO (Either String Settings)

  case msettings of
    Left err -> putStrLn err
    Right settings -> do
      print settings
      
      let connectInfo = defaultConnectInfo { connectHost     = dbHost     settings
                                           , connectPort     = fromIntegral
                                                             . dbPort   $ settings
                                           , connectUser     = dbUser     settings
                                           , connectPassword = dbPassword settings
                                           , connectDatabase = dbDatabase settings
                                           }

      pool <- runStderrLoggingT $ do
        p <- createMySQLPool connectInfo 1
        runSqlPool (runMigration migrateAll) p
        return p

      run 8080 (serve api (server pool))
