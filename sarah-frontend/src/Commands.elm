module Commands
  exposing (log)
--------------------------------------------------------------------------------
import Log.Message as Log
import Message            exposing (Message (..))
import Task               exposing (succeed, perform)
import Prelude            exposing (..)
--------------------------------------------------------------------------------

log : String -> Cmd Message
log msg = Task.perform identity identity (succeed . LogMessage . Log.Log $ msg)
