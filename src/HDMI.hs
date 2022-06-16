module HDMI where

import Clash.Prelude

import Hdmi.Init
import Hdmi.I2C
import Hdmi.I2C.Types

{-# ANN topEntity
  (Synthesize
    { t_name     = "HDMIComponent"
    , t_inputs   = [ PortProduct "" [ PortName "clk_25"
                                    , PortName "arst"
                                    , PortName "en"
                                    , PortName "rst"
                                    , PortName "pcsda"
                                    , PortName "iR"
                                    , PortName "iG"
                                    , PortName "iB"
                                    , PortName "hs"
                                    , PortName "vs"
                                    , PortName "de"
                                    ]
                   ]
    , t_output  = PortProduct ""[ PortName "oR"
                                , PortName "oG"
                                , PortName "oB"
                                , PortName "oVS"
                                , PortName "oHS"
                                , PortName "oDE"
                                , PortName "sclo"
                                , PortName "I2C_SCLK"
                                , PortName "sdao"
                                , PortName "sdaoEn"
                                ]
}) #-}
topEntity
  :: ( Clock System
     , Reset System
     , Enable System
     , Signal System Bool
     , Signal System Bit
     , Signal System (BitVector 10)
     , Signal System (BitVector 10)
     , Signal System (BitVector 10)
     , Signal System Bit
     , Signal System Bit
     , Signal System Bit
     ) ->
       ( Signal System Bool
       , Signal System Bool
       , Signal System (BitVector 10)
       , Signal System (BitVector 10)
       , Signal System (BitVector 10)
       , Signal System Bit
       , Signal System Bit
       , Signal System Bit
       , Signal System I2COut
       )
topEntity (clk, arst, en, rst, sdaI, ir, ig ,ib, ihs, ivs, ide) = (done, fault, or, og, ob, ohs, ovs, ode, i2cO)
  where
    (dout,hostAck,busy,al,ackOut,i2cO) = i2c clk arst rst (pure True) 250 start stop (pure False) write (pure False) din (bundle (scl,sdaI))
    (_,sclOen,_,_) = unbundle i2cO
    scl  = fmap bitCoerce sclOen
    (start,stop,write,din,done,fault) = hdmiConfig clk arst enableGen (rst,(pure True),hostAck,ackOut,al)
    or = ir
    og = ig
    ob = ib
    ovs = ivs
    ohs = ihs
    ode = ide
