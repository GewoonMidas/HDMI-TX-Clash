module Hdmi.I2CSlave where

import Clash.Prelude
import Clash.Explicit.SimIO

-- data HDMIConfTestS = HDMICTS { regFile    :: Vec 16 (Unsigned 8)
--                              , addr       :: Vec 8 Bit
--                              , cntr       :: Int
--                              , atStateM   :: HDMITestSM
--                              , prevSCL    :: Bit
--                              , prevSDA    :: Bit
--                              , sdaOut     :: Bit
--                              , regAddr    :: Unsigned 8
--                              }

-- data HDMITestSM = ATidle |
--                   ATaddr | ATaddrAck |
--                   ATreg  | ATregAck  |
--                   ATval  | ATvalAck  |
--                   ATstop
--                   deriving Show

-- type HDMIConfTestI = (Bit, Bit)
-- type HDMIConfTestO = (Bit, Vec 16 (Unsigned 8))

-- i2cSlaveInit :: HDMIConfTestS
-- i2cSlaveInit = HDMICTS (replicate d16 0x00) (replicate d8 0) 0 ATidle high high high 0

-- i2cSlaveT :: Reg HDMIConfTestS -> HDMIConfTestI -> SimIO HDMIConfTestO
-- i2cSlaveT s0 (scl,sda) = do
--   s <- readReg s0

--   let HDMICTS regFile addr cntr atStateM prevSCL prevSDA sdaOut regAddr = s

--   let startCondition = (prevSDA == high && sda == low) && scl == high
--       stopCondition = (prevSDA == low && sda == high) && scl == high

--       sclRising = prevSCL == low && scl == high
--       validAddr = pack addr == 0x98
--       validRegAddr = (pack addr >= 0 || pack addr <= 1) && lsb addr == low

--   stateMachine <- case atStateM of
--     ATidle
--       | startCondition -> do display "start"
--                              pure s {atStateM = ATaddr}

--     ATaddr
--       | cntr == 8 -> if validAddr then do
--                         display "valid slave address"
--                         pure s { atStateM = ATaddrAck
--                                , addr = repeat low
--                                , cntr = 0
--                                }
--                       else do
--                         display "invalid slave address"
--                         pure s { atStateM = ATidle
--                                , addr = repeat low
--                                , cntr = 0
--                                }
--       | sclRising -> pure s { cntr = cntr + 1
--                             , addr = addr <<+ sda
--                             , sdaOut = high
--                             }
--     ATaddrAck
--       | sclRising -> do display "addrAck"
--                         pure s { atStateM = ATreg
--                                , sdaOut = low
--                                }

--     ATreg
--       | cntr == 0 -> if validRegAddr then do
--                         display "valid register address"
--                         pure s { atStateM = ATregAck
--                                , addr = repeat low
--                                , cntr = 0
--                                , regAddr = shiftR (bitCoerce addr) 1
--                                }
--                       else do
--                         display "invalid register address"
--                         pure s { atStateM = ATidle
--                                , addr = repeat low
--                                , cntr = 0
--                                }
--       | sclRising -> pure s { cntr = cntr + 1
--                             , addr = addr <<+ sda
--                             , sdaOut = high
--                             }
--     ATregAck
--       | sclRising -> do display "regAck"
--                         pure s { sdaOut = low
--                                , atStateM = ATval
--                                }

--     ATval
--       | cntr == 8 -> do display "valid value"
--                         pure s { atStateM = ATvalAck
--                                , addr = repeat low
--                                , cntr = 0
--                                , regFile = replace regAddr (bitCoerce addr) regFile
--                                }
--       | sclRising -> pure s { cntr = cntr + 1
--                             , addr = addr <<+ sda
--                             , sdaOut = high }

--     ATvalAck
--       | sclRising -> do display "valAck"
--                         pure s { atStateM = ATstop
--                                , sdaOut = low
--                                }

--     ATstop
--       | stopCondition -> do display "stop"
--                             pure s { atStateM = ATidle
--                                    , sdaOut = high
--                                    }

--     _ -> pure s

--   writeReg s0 (stateMachine { prevSDA = sda
--                             , prevSCL = scl
--                             })
--   pure (sdaOut, regFile)

-- i2cSlave
--   :: Clock System
--   -> Signal System HDMIConfTestI
--   -> Signal System HDMIConfTestO
-- i2cSlave clk = mealyIO clk i2cSlaveT (reg i2cSlaveInit)
-- {-# NOINLINE i2cSlave #-}

-- import Clash.Prelude
-- import Clash.Explicit.SimIO

data HDMIConfTestS = HCTS{ regFile  :: Vec 99 (Unsigned 8)
                         , addr     :: Vec 8 Bit
                         , cntr     :: Int
                         , atStateM :: HdmiTestSM
                         , prevSCL  :: Bit
                         , prevSDA  :: Bit
                         , sdaOut   :: Bit
                         , regAddr  :: Unsigned 8
                         }

data HdmiTestSM = ATidle |
                   ATaddr | ATaddrAck |
                   ATreg  | ATregAck  |
                   ATval  | ATvalAck  |
                   ATstop
  deriving Show

type HDMIConfTestI = (Bit,Bit)
type HDMIConfTestO = (Bit,Vec 99 (Unsigned 8))

i2cSlaveInit :: HDMIConfTestS
i2cSlaveInit = HCTS (repeat 0x0) (replicate d8 0) 0 ATidle high high high 0

i2cSlaveT :: Reg HDMIConfTestS -> HDMIConfTestI -> SimIO HDMIConfTestO
i2cSlaveT s0 (scl,sda) = do
  s <- readReg s0

  let HCTS regFile addr cntr atStateM prevSCL prevSDA sdaOut regAddr = s

  let startCondition = (prevSDA == high && sda == low) && scl == high
      stopCondition  = (prevSDA == low && sda == high) && scl == high

      sclRising    = prevSCL == low && scl == high
      validAddr    = pack addr == 0x98
      validRegAddr = (pack addr >= 0 || pack addr <= 1) && lsb addr == low

  stateMachine <- case atStateM of
    ATidle
      | startCondition -> do display "start"
                             pure s {atStateM = ATaddr}
    ATaddr
      | cntr == 8 -> if validAddr then do
                       display "valid addr"
                       pure s { atStateM = ATaddrAck
                              , addr = repeat low
                              , cntr = 0 }
                     else do
                       display "invalid addr"
                       pure s { atStateM = ATidle
                              , addr = repeat low
                              , cntr = 0}
      | sclRising ->   pure s { cntr = cntr + 1
                              , addr = addr <<+ sda
                              , sdaOut = high }
    ATaddrAck
      | sclRising -> do display "addrAck"
                        pure s { atStateM = ATreg, sdaOut = low }
    ATreg
      | cntr == 8 -> if validRegAddr then do
                       display "valid reg addr"
                       pure s { atStateM = ATregAck
                              , addr     = repeat low
                              , cntr     = 0
                              , regAddr  = shiftR (bitCoerce addr) 1
                              }
                     else do
                       display "invalid reg addr"
                       pure s { atStateM = ATidle
                              , addr = repeat low
                              , cntr = 0
                              }
      | sclRising -> pure s { cntr = cntr + 1
                            , addr = addr <<+ sda
                            , sdaOut = high }
    ATregAck
      | sclRising -> do display "regAck"
                        pure s { sdaOut = low
                               , atStateM = ATval
                               }
    ATval
      | cntr == 8 -> do display "val"
                        pure s { atStateM = ATvalAck
                               , addr = repeat low
                               , cntr = 0
                               , regFile = replace regAddr (bitCoerce addr) regFile
                               }
      | sclRising -> pure s { cntr = cntr + 1
                            , addr = addr <<+ sda
                            , sdaOut = high }
    ATvalAck
      | sclRising -> do display "valAck"
                        pure s { sdaOut = low
                               , atStateM = ATstop
                               }
    ATstop
      | stopCondition -> do display "stop"
                            pure s { atStateM = ATidle
                                   , sdaOut = high
                                   }
    _ -> pure s

  writeReg s0 (stateMachine {prevSDA = sda, prevSCL = scl})
  pure (sdaOut, regFile)

{-# ANN i2cSlave Synthesize { t_name = "slave", t_inputs = [], t_output = PortName "" } #-}
i2cSlave
  :: Clock System
  -> Signal System HDMIConfTestI
  -> Signal System HDMIConfTestO
i2cSlave clk = mealyIO clk i2cSlaveT (reg i2cSlaveInit)
{-# NOINLINE i2cSlave #-}
