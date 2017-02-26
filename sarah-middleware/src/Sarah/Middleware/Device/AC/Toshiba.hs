{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Control.Concurrent          (forkIO)
import Control.Distributed.Process
import Data.Aeson
import Data.Aeson.Types            (typeMismatch)
import Data.Bits                   (Bits, testBit, xor, zeroBits)
import Data.ByteString             (ByteString)
import Data.Monoid                 ((<>))
import Data.Text                   (unpack)
import Import.DeriveJSON
import Import.MkBinary
import Raspberry.GPIO
import Sarah.Middleware.Model      (IsDevice (..), PortManager, DeviceController (..))
--------------------------------------------------------------------------------
import qualified Data.ByteString   as BS
import qualified Language.C.Inline as C
--------------------------------------------------------------------------------

data ToshibaAC = ToshibaAC Pin deriving (Show)

instance IsDevice ToshibaAC where
  type DeviceState ToshibaAC = Config

  data DeviceCommand ToshibaAC = SetTemperature Temperature
                               | GetTemperature
                               | SetFanMode     Fan
                               | GetFanMode
                               | SetMode        Mode
                               | GetMode
                               | SetPowerMode   (Maybe Power)
                               | GetPowerMode

  startDeviceController (ToshibaAC pin) portManager = do
    say "[ToshibaAC.startDeviceController]"
    DeviceController <$> spawnLocal (controller portManager pin defaultConfig)

      where
        controller :: PortManager -> Pin -> Config -> Process ()
        controller portManager pin config = receiveWait [ matchAny $ \m -> do
                                                            say $ "[ToshibaAC] Received unexpected message " ++ show m
                                                            controller portManager pin config
                                                        ]

instance ToJSON ToshibaAC where
  toJSON (ToshibaAC (Pin pin)) = object [ "model" .= String "ToshibaAC"
                                        , "gpio"  .= toJSON pin
                                        ]

instance FromJSON ToshibaAC where
  parseJSON = withObject "ToshibaAC" $ \o -> do
    model <- o .: "model" :: Parser Text
    case model of
      "ToshibaAC" -> ToshibaAC <$> (Pin <$> o .: "gpio")
      model       -> fail $ "Invalid model identifier: " ++ unpack model


data Temperature = T17 | T18 | T19 | T20 | T21 | T22 | T23 | T24 | T25 | T26 | T27 | T28 | T29 | T30 deriving (Binary, Generic, Typeable)
data Fan         = FanAuto | FanQuiet | FanVeryLow | FanLow | FanNormal | FanHigh | FanVeryHigh      deriving (Binary, Generic, Typeable)
data Mode        = ModeAuto | ModeCool | ModeDry | ModeFan | ModeOff                                 deriving (Binary, Generic, Typeable)
data Power       = PowerHigh | PowerEco                                                              deriving (Binary, Generic, Typeable)

data Config = Config { temperature :: Temperature
                     , fan         :: Fan
                     , mode        :: Mode
                     , mpower      :: Maybe Power
                     }
  deriving (Binary, Generic, Typeable)

defaultConfig :: Config
defaultConfig = Config { temperature = T22
                       , fan         = FanAuto
                       , mode        = ModeAuto
                       , mpower      = Nothing
                       }

instance ToBits Temperature where
  toBits T17 = 0x0
  toBits T18 = 0x1
  toBits T19 = 0x2
  toBits T20 = 0x3
  toBits T21 = 0x4
  toBits T22 = 0x5
  toBits T23 = 0x6
  toBits T24 = 0x7
  toBits T25 = 0x8
  toBits T26 = 0x9
  toBits T27 = 0xA
  toBits T28 = 0xB
  toBits T29 = 0xC
  toBits T30 = 0xD

instance ToBits Fan where
  toBits FanAuto     = 0x0
  toBits FanQuiet    = 0x2
  toBits FanVeryLow  = 0x4
  toBits FanLow      = 0x6
  toBits FanNormal   = 0x8
  toBits FanHigh     = 0xA
  toBits FanVeryHigh = 0xC

instance ToBits Mode where
  toBits ModeAuto = 0x0
  toBits ModeCool = 0x1
  toBits ModeDry  = 0x2
  toBits ModeFan  = 0x4
  toBits ModeOff  = 0x7

instance ToBits Power where
  toBits PowerHigh = 0x1
  toBits PowerEco  = 0x3

--------------------------------------------------------------------------------
deriveJSON jsonOptions ''Temperature
deriveJSON jsonOptions ''Fan
deriveJSON jsonOptions ''Mode
deriveJSON jsonOptions ''Power
deriveJSON jsonOptions ''Config
--------------------------------------------------------------------------------

C.context (C.baseCtx <> C.bsCtx)
C.include "<stdlib.h>"
C.include "<stdio.h>"
C.include "irslinger.h"

bitsToNibble :: (Bits a) => a -> ByteString
bitsToNibble b = BS.concat [ if testBit b 3 then "1" else "0"
                           , if testBit b 2 then "1" else "0"
                           , if testBit b 1 then "1" else "0"
                           , if testBit b 0 then "1" else "0"
                           ]


convert :: Config -> ByteString
convert Config{..} =
  let t = toBits temperature :: Int
      f = toBits fan         :: Int
      m = toBits mode        :: Int
      bits = case mpower of
               Nothing    -> let checksum = map (foldr xor zeroBits) [[t, f], [0x1, m   ]]
                             in [0xF, 0x2, 0x0, 0xD, 0x0, 0x3, 0xF, 0xC, 0x0, 0x1, t, 0x0, f, m, 0x0, 0x0        ] ++ checksum
               Just power -> let p        = toBits power :: Int
                                 checksum = map (foldr xor zeroBits) [[t, f], [0x9, m, p]]
                             in [0xF, 0x2, 0x0, 0xD, 0x0, 0x4, 0xF, 0xB, 0x0, 0x9, t, 0x0, f, m, 0x0, 0x0, 0x0, p] ++ checksum
  in BS.concat . map bitsToNibble $ bits


send :: Pin -> Config -> IO ()
send (Pin pin) config = do
  let bs = convert config
  res <- [C.block| int
           {
             int frequency = 38000;          // The frequency of the IR signal in Hz
             double dutyCycle = 0.5;         // The duty cycle of the IR signal. 0.5 means for every cycle,
                                             // the LED will turn on for half the cycle time, and off the other half

             int* codes = (int*) calloc(4 * $bs-len:bs + 7, sizeof(int));

             if (!codes)
             {
               printf("Memory allocation for sending IR signals failed!");
               return -1;
             }

             char c = '_';
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

             codes[bsIdx++] =  550;
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

             codes[bsIdx++] = 550;

             int result = irSlingRaw( $(int pin)
                                    , frequency
                                    , dutyCycle
                                    , codes
                                    , bsIdx
                                    );

             if (codes)
               free(codes);

             return result;
           }
         |]
  return ()

--------------------------------------------------------------------------------
