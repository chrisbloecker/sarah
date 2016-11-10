{-# LANGUAGE QuasiQuotes     #-}
--------------------------------------------------------------------------------
module Device.AC.Toshiba
  where
--------------------------------------------------------------------------------
import           Foreign.C.Types
--------------------------------------------------------------------------------
import qualified Language.C.Inline as C
--------------------------------------------------------------------------------

data Temperature = T17 | T18 | T19 | T20 | T21 | T22 | T23 | T24 | T25 | T26 | T27 | T28 | T29 | T30
data Fan         = FanAuto | FanVeryLow | FanLow | FanNormal | FanHigh | FanVeryHigh
data Mode        = ModeAuto | ModeCool | ModeDry | ModeFan | ModeOff
data Power       = PowerHigh | PowerEco

data Config = Config { temperature :: Temperature
                     , fan         :: Fan
                     , mode        :: Mode
                     , power       :: Maybe Power
                     }

--------------------------------------------------------------------------------

mycos :: CDouble -> IO CDouble
mycos x = [C.exp| double{ cos($(double x)) } |]
