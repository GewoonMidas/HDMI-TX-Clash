{-# LANGUAGE RecordWildCards, ViewPatterns, PartialTypeSignatures #-}
module Hdmi.Init where

import Clash.Prelude
import Debug.Trace

data HDMIStateMachine = HDMICONFena  |
                        HDMICONFaddr | HDMICONFaddrAck |
                        HDMICONFreg  | HDMICONFregAck  |
                        HDMICONFdata | HDMICONFdataAck |
                        HDMICONFstop
                        deriving (Generic, NFDataX)

data HdmiConfS = HdmiConfS { hdmiConfStateM :: HDMIStateMachine
                           , start          :: Bool
                           , stop           :: Bool
                           , write          :: Bool
                           , din            :: Vec 8 Bit
                           , fault          :: Bool
                           , lutIndex       :: Index 8
                           } deriving (Generic, NFDataX)

type HDMIConfI = (Bool, Bool, Bool, Bool, Bool)
type HDMIConfO = (Bool, Bool, Bool, BitVector 8, Bool, Bool)

hdmiConfig
  :: Clock System
  -> Reset System
  -> Enable System
  -> Unbundled System HDMIConfI
  -> Unbundled System HDMIConfO
hdmiConfig = exposeClockResetEnable (mealyB hdmiConfigT hdmiConfigInit)

hdmiConfigInit :: HdmiConfS
hdmiConfigInit = HdmiConfS { hdmiConfStateM = HDMICONFena
                           , start          = False
                           , stop           = False
                           , write          = False
                           , din            = repeat low
                           , lutIndex       = 0
                           , fault          = False
                           }

hdmiConfigT :: (HdmiConfS -> HDMIConfI -> (HdmiConfS, HDMIConfO))
hdmiConfigT state@(HdmiConfS{..}) input = (state', output)
  where

    (rst,ena,cmdAck,rxAck,al) = input
    output = (start,stop,write,v2bv din,done,fault)

    i2cSlvAddr = 0x98 :: BitVector 8

    success = cmdAck && (not al)

    done = lutIndex == (fromIntegral $ length lut)

    lutData = configLut lut lutIndex

    state' = if rst then
        hdmiConfigInit
      else
        case hdmiConfStateM of
          HDMICONFena -> if ena && (not done) then
              state { hdmiConfStateM = HDMICONFaddr }
            else
              state


          HDMICONFaddr ->
              state { hdmiConfStateM = HDMICONFaddrAck
                    , start = True
                    , write = True
                    , din = bv2v i2cSlvAddr
                    }


          HDMICONFaddrAck -> if success then
              state { hdmiConfStateM = HDMICONFreg
                    , start = False
                    , write = False
                    }
            else
              state


          HDMICONFreg -> if (rxAck == False) then
              state { hdmiConfStateM = HDMICONFregAck
                    , write = True
                    , din = unpack (fst lutData)
                    , fault = False
                    }
            else
              state { hdmiConfStateM = HDMICONFena
                    , fault = True
                    }


          HDMICONFregAck -> if success then
              state { hdmiConfStateM = HDMICONFdata
                    , write = False
                    }
            else
              state

          HDMICONFdata -> if (rxAck == False) then do
              state { hdmiConfStateM = HDMICONFdataAck
                    , write = True
                    , stop = True
                    , din = unpack (snd lutData)
                    , fault = False
                    }
            else
              state { hdmiConfStateM = HDMICONFena
                    , fault = True
                    }

          HDMICONFdataAck -> if success then
              state { hdmiConfStateM = HDMICONFstop
                    , write = False
                    , stop = False
                    }
            else
              state

          HDMICONFstop -> if (rxAck == False) then
              state { hdmiConfStateM = HDMICONFena
                    , lutIndex = lutIndex + 1
                    , fault = False
                    }
            else
              state { hdmiConfStateM = HDMICONFena
                    , fault = True
                    }

configLut :: (KnownNat n, KnownNat m) => Vec n (BitVector 8, BitVector 8) -> Index m -> (BitVector  8, BitVector 8)
configLut l i
  | i > (fromIntegral $ length l) = (0x04, 0b00000000)
  | otherwise = l !! i


lut :: Vec 8 ( BitVector 8, BitVector 8)
lut = (0x00, 0b00010000) :>
      (0x08, 0b00000000) :>
      (0x08, 0x10) :>
      (0xC2, 0x10) :>
      (0x08, 0b00000000) :>
      (0x08, 0b00000100) :>
      (0x08, 0b00000000) :>
      (0xC2, 0b00000000) :>
      Nil
