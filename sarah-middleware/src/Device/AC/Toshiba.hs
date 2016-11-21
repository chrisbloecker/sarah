{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
module Device.AC.Toshiba
  where
--------------------------------------------------------------------------------
import           Control.Concurrent    (forkIO)
import           Data.Bits
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Monoid           ((<>))
import           Raspberry.GPIO
--------------------------------------------------------------------------------
import qualified Data.ByteString   as BS
import qualified Language.C.Inline as C
--------------------------------------------------------------------------------

data Temperature = T17 | T18 | T19 | T20 | T21 | T22 | T23 | T24 | T25 | T26 | T27 | T28 | T29 | T30
data Fan         = FanAuto | FanQuiet | FanVeryLow | FanLow | FanNormal | FanHigh | FanVeryHigh
data Mode        = ModeAuto | ModeCool | ModeDry | ModeFan | ModeOff
data Power       = PowerHigh | PowerEco

data Config = Config { temperature :: Temperature
                     , fan         :: Fan
                     , mode        :: Mode
                     , mpower      :: Maybe Power
                     }

--------------------------------------------------------------------------------

C.context (C.baseCtx <> C.bsCtx)
C.include "irslinger.h"
C.include "<stdio.h>"

bitsToNibble :: (Bits a) => a -> ByteString
bitsToNibble b = BS.concat [ if testBit b 3 then "1" else "0"
                           , if testBit b 2 then "1" else "0"
                           , if testBit b 1 then "1" else "0"
                           , if testBit b 0 then "1" else "0"
                           ]

convert :: Config -> ByteString
convert Config{..} =
  let t = case temperature of
            T17 -> 0x0 :: Int
            T18 -> 0x1
            T19 -> 0x2
            T20 -> 0x3
            T21 -> 0x4
            T22 -> 0x5
            T23 -> 0x6
            T24 -> 0x7
            T25 -> 0x8
            T26 -> 0x9
            T27 -> 0xA
            T28 -> 0xB
            T29 -> 0xC
            T30 -> 0xD
      f = case fan of
            FanAuto     -> 0x0
            FanQuiet    -> 0x2
            FanVeryLow  -> 0x4
            FanLow      -> 0x6
            FanNormal   -> 0x8
            FanHigh     -> 0xA
            FanVeryHigh -> 0xC
      m = case mode of
            ModeAuto -> 0x0
            ModeCool -> 0x1
            ModeDry  -> 0x2
            ModeFan  -> 0x4
            ModeOff  -> 0x7
  in BS.concat . map bitsToNibble $ [0xF, 0x2, 0x0, 0xD, 0x0]
                         ++ case mpower of
                              Nothing    -> [0x3, 0xF, 0xC, 0x0, 0x1, t, 0x0, f, m, 0x0, 0x0, foldr xor zeroBits [t, f], foldr xor zeroBits [0x1, m]]
                              Just power -> let p = case power of
                                                    PowerHigh -> 0x1
                                                    PowerEco  -> 0x3
                                            in [0x4, 0xF, 0xB, 0x0, 0x9, t, 0x0, f, m, 0x0, 0x0, 0x0, p, foldr xor zeroBits [t, f], foldr xor zeroBits [0x9, m, p]]

send :: Pin -> ByteString -> IO ()
send (Pin pin) bs = do
  res <- [C.block| int
           {
             int frequency = 38000;          // The frequency of the IR signal in Hz
             double dutyCycle = 0.5;         // The duty cycle of the IR signal. 0.5 means for every cycle,
                                             // the LED will turn on for half the cycle time, and off the other half

             int* codes = (int*) calloc(4 * $bs-len:bs + 5, sizeof(int));

             if (!codes)
             {
               printf("Memory allocation for sending IR signals failed!");
               return -1;
             }

             char c;
             int i
               , bsIdx = 0
               ;

             codes[bsIdx++] = 4380;
             codes[bsIdx++] = 4360;

             for (i = 0; i < $bs-len:bs; ++i)
               switch ($bs-ptr:bs[i])
               {
                  case '0':
                    codes[bsIdx++] = 550;
                    codes[bsIdx++] = 530;
                    break;
                  case '1':
                    codes[bsIdx++] = 550;
                    codes[bsIdx++] = 1600;
                    break;
                  default:
                    printf("Invalid character in bitstring: %c", $bs-ptr:bs[i]);
               }

             codes[bsIdx++] = 5470;
             codes[bsIdx++] = 4380;
             codes[bsIdx++] = 4360;

             for (i = 0; i < $bs-len:bs; ++i)
               switch ($bs-ptr:bs[i])
               {
                 case '0':
                   codes[bsIdx++] = 550;
                   codes[bsIdx++] = 530;
                   break;
                 case '1':
                   codes[bsIdx++] = 550;
                   codes[bsIdx++] = 1600;
                   break;
                 default:
                   printf("Invalid character in bitstring: %c", c);
               }

             int result = irSlingRaw( $(int pin)
                                    , frequency
                                    , dutyCycle
                                    , codes
                                    , bsIdx // 4 * $bs-len:bs + 5
                                    );

             if (codes)
               free(codes);

             return result;
           }
         |]
  return ()
