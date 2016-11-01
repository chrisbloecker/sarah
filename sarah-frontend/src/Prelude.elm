module Prelude
  exposing (..)
--------------------------------------------------------------------------------
import Debug       exposing (log)
import Json.Decode exposing (..)
import List        exposing (length)
import String      exposing (split, toInt)
--------------------------------------------------------------------------------

type alias Hour   = Int
type alias Minute = Int
type alias Second = Int

type alias Time = { hours   : Hour
                  , minutes : Minute
                  , seconds : Second
                  }

time : Decoder Time
time = let decodeToTime s = let ss = split ":" s
                            in case ss of
                              []         -> Err ("Invalid time: " ++ s)
                              [_]        -> Err ("Invalid time: " ++ s)
                              [_,_]      -> Err ("Invalid time: " ++ s)
                              [hh,mm,ss] -> case toInt hh of
                                              Err _   -> Err ("Invalid time: " ++ s)
                                              Ok hour -> case toInt mm of
                                                Err _     -> Err ("Invalid time: " ++ s)
                                                Ok minute -> case toInt ss of
                                                  Err _     -> Err ("Invalid time: " ++ s)
                                                  Ok second -> Ok (Time hour minute second)
                              _ -> Err ("Invalid time: " ++ s)
       in customDecoder string decodeToTime

--------------------------------------------------------------------------------

(.) : (b -> c) -> (a -> b) -> (a -> c)
(.) = (<<)

($) : (a -> b) -> a -> b
($) = (<|)

const : a -> (b -> a)
const = always

undefined : a
undefined = undefined

--------------------------------------------------------------------------------

logDebug = log "[DEBUG]"
logError = log "[ERROR]"
logInfo  = log "[INFO ]"
