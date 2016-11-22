module Util
  exposing (..)
--------------------------------------------------------------------------------
import Date exposing (Date, Month (..), year, day)
--------------------------------------------------------------------------------

toIsoDate : Date -> String
toIsoDate date = let yyyy = toString (year date)
                     mm   = month (Date.month date)
                     dd   = (if day date < 10 then "0" else "") ++ (toString (day date))
                 in yyyy ++ "-" ++ mm ++ "-" ++ dd

month : Month -> String
month m = case m of
  Jan -> "01"
  Feb -> "02"
  Mar -> "03"
  Apr -> "04"
  May -> "05"
  Jun -> "06"
  Jul -> "07"
  Aug -> "08"
  Sep -> "09"
  Oct -> "10"
  Nov -> "11"
  Dec -> "12"
