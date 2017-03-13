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
import Control.Distributed.Process
import Data.Aeson                  (ToJSON (..), FromJSON (..), (.=), (.:))
import Data.Aeson.Types            (Parser, Value (..), typeMismatch, object, withObject)
import Data.Bits                   (Bits, testBit, xor, zeroBits)
import Data.ByteString             (ByteString)
import Data.Monoid                 ((<>))
import Data.Text                   (Text, unpack)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
import Physics
import Raspberry.GPIO
import Sarah.Middleware.Model      (IsDevice (..), PortManager, DeviceController (..))
import Sarah.Middleware.Types      (FromPid (..), Query (..), getCommand, mkSuccess, mkError)
--------------------------------------------------------------------------------
import qualified Data.ByteString   as BS
import qualified Language.C.Inline as C
--------------------------------------------------------------------------------

data Temp  = T17 | T18 | T19 | T20 | T21 | T22 | T23 | T24 | T25 | T26 | T27 | T28 | T29 | T30 deriving (Generic, Typeable, ToJSON, FromJSON, Eq, Ord, Enum, Bounded)
data Fan   = FanAuto | FanQuiet | FanVeryLow | FanLow | FanNormal | FanHigh | FanVeryHigh      deriving (Generic, Typeable, ToJSON, FromJSON, Eq, Ord, Enum, Bounded)
data Mode  = ModeAuto | ModeCool | ModeDry | ModeFan | ModeOff                                 deriving (Generic, Typeable, ToJSON, FromJSON, Eq)
data Power = PowerHigh | PowerEco                                                              deriving (Generic, Typeable, ToJSON, FromJSON, Eq)

data Config = Config { temperature :: Temperature
                     , fan         :: Fan
                     , mode        :: Mode
                     , mpower      :: Maybe Power
                     }
  deriving (Generic, ToJSON, FromJSON)

defaultConfig :: Config
defaultConfig = Config { temperature = Temperature 22
                       , fan         = FanAuto
                       , mode        = ModeAuto
                       , mpower      = Nothing
                       }

instance ToBits Temp where
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
  let t = toBits . fromTemperature $ temperature :: Int
      f = toBits                     fan         :: Int
      m = toBits                     mode        :: Int
      bits = case mpower of
               Nothing    -> let checksum = map (foldr xor zeroBits) [[t, f], [0x1, m   ]]
                             in [0xF, 0x2, 0x0, 0xD, 0x0, 0x3, 0xF, 0xC, 0x0, 0x1, t, 0x0, f, m, 0x0, 0x0        ] ++ checksum
               Just power -> let p        = toBits power :: Int
                                 checksum = map (foldr xor zeroBits) [[t, f], [0x9, m, p]]
                             in [0xF, 0x2, 0x0, 0xD, 0x0, 0x4, 0xF, 0xB, 0x0, 0x9, t, 0x0, f, m, 0x0, 0x0, 0x0, p] ++ checksum
  in BS.concat . map bitsToNibble $ bits


toTemperature :: Temp -> Temperature
toTemperature T17 = Temperature 17
toTemperature T18 = Temperature 18
toTemperature T19 = Temperature 19
toTemperature T20 = Temperature 20
toTemperature T21 = Temperature 21
toTemperature T22 = Temperature 22
toTemperature T23 = Temperature 23
toTemperature T24 = Temperature 24
toTemperature T25 = Temperature 25
toTemperature T26 = Temperature 26
toTemperature T27 = Temperature 27
toTemperature T28 = Temperature 28
toTemperature T29 = Temperature 29
toTemperature T30 = Temperature 30


fromTemperature :: Temperature -> Temp
fromTemperature (Temperature t) | t < 17.5  = T17
                                | t < 18.5  = T18
                                | t < 19.5  = T19
                                | t < 20.5  = T20
                                | t < 21.5  = T21
                                | t < 22.5  = T22
                                | t < 23.5  = T23
                                | t < 24.5  = T24
                                | t < 25.5  = T25
                                | t < 26.5  = T26
                                | t < 27.5  = T27
                                | t < 28.5  = T28
                                | t < 29.5  = T29
                                | otherwise = T30


data ErrorCode = Ok | Error

-- ToDo: use an enum in C for better error reporting
setAC :: Pin -> Config -> IO ErrorCode
setAC (Pin pin) config = do
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
  return $ case res of
    0 -> Ok
    _ -> Error

--------------------------------------------------------------------------------

newtype ToshibaAC = ToshibaAC Pin deriving (Show)

instance IsDevice ToshibaAC where
  type DeviceState ToshibaAC = Config

  data DeviceCommand ToshibaAC = SetTemperature Temperature
                               | SetFanMode     Fan
                               | SetMode        Mode
                               | SetPowerMode   (Maybe Power)
                               | GetConfig
                               | PowerOn
                               | PowerOff
                               | UpTemperature
                               | DownTemperature
                               | UpFan
                               | DownFan
    deriving (Generic, ToJSON, FromJSON)

  startDeviceController (ToshibaAC pin) portManager = do
    say "[ToshibaAC.startDeviceController]"
    DeviceController <$> spawnLocal (controller defaultConfig portManager pin)

      where
        controller :: Config -> PortManager -> Pin -> Process ()
        controller config@Config{..} portManager pin =
          receiveWait [ match $ \(FromPid src Query{..}) -> case getCommand queryCommand of
                          Left err -> say $ "[ToshibaAC.controller] Can't decode command: " ++ err
                          Right command -> case command of
                            SetTemperature t -> do
                              let config' = config { temperature = t }
                              res <- liftIO $ setAC pin config'
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller config' portManager pin

                            SetFanMode f -> do
                              let config' = config { fan = f }
                              res <- liftIO $ setAC pin config'
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller config' portManager pin

                            SetMode m -> do
                              let config' = config { mode = m }
                              res <- liftIO $ setAC pin config'
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller config' portManager pin

                            SetPowerMode mp -> do
                              let config' = config { mpower = mp }
                              res <- liftIO $ setAC pin config'
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller config' portManager pin

                            GetConfig -> do
                              send src (mkSuccess config)
                              controller config portManager pin

                            PowerOn -> do
                              res <- liftIO $ setAC pin defaultConfig
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller defaultConfig portManager pin

                            PowerOff -> do
                              let config' = config { mode = ModeOff }
                              res <- liftIO $ setAC pin config'
                              case res of
                                Ok    -> emptyReply src
                                Error -> send src (mkError "")
                              controller config' portManager pin

                            UpTemperature -> if temperature >= toTemperature maxBound
                              then do
                                emptyReply src
                                controller config portManager pin
                              else do
                                let temperature' = Temperature (getTemperature temperature + 1)
                                    config' = config { temperature = temperature' }
                                res <- liftIO $ setAC pin config'
                                case res of
                                  Ok    -> emptyReply src
                                  Error -> send src (mkError "")
                                controller config' portManager pin

                            DownTemperature -> if temperature <= toTemperature minBound
                              then do
                                emptyReply src
                                controller config portManager pin
                              else do
                                let temperature' = Temperature (getTemperature temperature - 1)
                                    config' = config { temperature = temperature' }
                                res <- liftIO $ setAC pin config'
                                case res of
                                  Ok    -> emptyReply src
                                  Error -> send src (mkError "")
                                controller config' portManager pin

                            UpFan -> if fan == maxBound
                              then do
                                emptyReply src
                                controller config portManager pin
                              else do
                                let config' = config { fan = succ fan }
                                res <- liftIO $ setAC pin config'
                                case res of
                                  Ok    -> emptyReply src
                                  Error -> send src (mkError "")
                                controller config' portManager pin

                            DownFan -> if fan == FanQuiet
                              then do
                                emptyReply src
                                controller config portManager pin
                              else do
                                let config' = config { fan = pred fan }
                                res <- liftIO $ setAC pin config'
                                case res of
                                  Ok -> emptyReply src
                                  Error -> send src (mkError "")
                                controller config' portManager pin

                      , matchAny $ \m -> do
                          say $ "[ToshibaAC] Received unexpected message " ++ show m
                          controller config portManager pin
                      ]

emptyReply :: ProcessId -> Process ()
emptyReply dest = send dest (mkSuccess ())

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
