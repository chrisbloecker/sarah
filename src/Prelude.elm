module Prelude
  exposing (..)
--------------------------------------------------------------------------------
import Json.Decode exposing (..)
import List        exposing (length)
import String      exposing (split)
--------------------------------------------------------------------------------

type Either a b = Left a | Right b

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
                            in if length ss == 3
                                 then let hour   = 1
                                          minute = 2
                                          second = 3
                                      in Ok (Time hour minute second)
                                 else Err ("Invalid format for time: " ++ s)
       in customDecoder string decodeToTime

--------------------------------------------------------------------------------

undefined : a
undefined = undefined
