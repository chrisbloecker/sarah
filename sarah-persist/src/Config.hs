module Config
  where
--------------------------------------------------------------------------------
import Database.Persist.Sql       (ConnectionPool)
--------------------------------------------------------------------------------

data Config = Config { getPool :: ConnectionPool }
