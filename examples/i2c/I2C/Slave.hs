-- * A bus-slave implementation

-- Restrictions:
-- - 7bit address space
-- - missing debouncer
-- - one baud rate only
-- - NACK responses missing

-- * Preliminaries

{-# LANGUAGE GADTSyntax, PatternSynonyms, ViewPatterns, FlexibleInstances #-}

module Slave where

import CLaSH.Prelude
import Debug.Trace (traceShowId)
import Data.Monoid

-- * Types

data State where
  Start :: State
  Collect :: Unsigned 3 -> Unsigned 8 -> State
  Stop :: Unsigned 8 -> State

-- * Flank detection

-- ** Level and derivative

pattern HIGH = (1, False)
pattern LOW = (0, False)
pattern UP = (1, True)
pattern DOWN = (0, True)

-- ** I2C events

pattern START = (DOWN, HIGH)
pattern STOP = (UP, HIGH)
pattern ACK = (UP, LOW)



--                              +-- SDA      +--- SCL
--                              |    +- SDA' |    +- SCL'
--                              v    v       v    v
flank :: Maybe (Unsigned 4) -> ((Bit, Bool), (Bit, Bool)) -> (Maybe (Unsigned 4), Bool, Maybe Bit, Bool)
flank Nothing START = (Just 0, True, Nothing, False)
flank Nothing STOP = (Nothing, False, Nothing, True)
--flank Nothing ACK = (Just 0, False, Nothing, False) -- sense this only when sending (after bit #7)
flank (Just n) ((sda, _), UP) = (Just $ n+1, False, Just sda, False)
flank (Just 8) (_, DOWN) = (Nothing, False, Nothing, False)
flank s@Just{} (_, _) = (s, False, Nothing, False)
flank _ _ = (Nothing, False, Nothing, False) -- STOP-like

-- * Transfer function for derivatives

derive :: Eq a => a -> a -> (a, (a, Bool))
derive a0 a = (a, (a, a /= a0))

-- * Isolate flanks of the two wires

sda'scl' :: Signal Bit -> Signal Bit -> Signal ((Bit, Bool), (Bit, Bool))
sda'scl' sda scl = bundle (derived sda, derived scl)
  where derived = mealy derive 1

-- * Transfer function for the protocol

class I2C state where
  protocol :: state -> ((Bit, Bool), (Bit, Bool)) -> (state, Bool)


-- ** Some simple implementations

instance I2C (Unsigned 8) where
  protocol byte s = (byte, {- cnt==7 && -} not start && not stop)
    where (cnt, start, bit, stop) = flank Nothing s

-- ** State diagram for byte transfers

data Transfer = Wait
              | Read (Unsigned 7) | Write (Unsigned 7)
              | Receive (Unsigned 8) | Send (Unsigned 8)

-- state transitions:
-- Wait -> Write -> Receive* -> Wait
-- Wait -> Read -> Send* -> Wait

instance I2C Transfer where
  protocol tr s = case (tr, cnt, bit) of
                    (Wait, Just 0, Nothing) -> (Read 0, ack)
                    (Read n, Just 6, Just 0) -> (Write n, ack)
                    (Read n, Just 7, Just 0) -> (Read n, ack)
                    (Read n, Just _, Just b) -> (Read $ truncateB $ bitCoerce (n,b), ack) -- shift!
    where (cnt, start, bit, stop) = flank Nothing s
          ack = not start && not stop && byte
          byte = case cnt of Just 7 -> True; _ -> False


-- * Tests

scl8 = moore up (<4) (0 :: Unsigned 3) (pure ())
  where up 7 () = 0
        up n () = n + 1


-- * The complete machine

-- Now it's time to connect the pieces.
-- The output is True: generate ack

i2c :: I2C state => state -> Signal Bit -> Signal Bit -> Signal Bool
i2c init sda scl = mealy protocol init (sda'scl' sda scl)


-- * This comes from another concept study

-- See https://github.com/ggreif/clash-ground/blob/master/I2CServant.hs

bitSlave :: Signal ((Bit, Bool), (Bit, Bool)) -- SDA, SCL + flanks
         -> Signal (Unsigned 8) -- byte to write
         -> Signal Bool -- ACK-out
         -> Signal (Unsigned 8, (Bool, Bool, Bool, Bool), Bit) -- byte read, (START, ACK, NACK, ReSTART), SDA-out
bitSlave a b c = mealy spin (Nothing, 0 :: Unsigned 8) (bundle (a, b, c))
  where spin (seq, byte) (diffd, wrbyte, ack) = ((seq', byte'), out)
          where (seq', start, rdbit, stop) = flank seq diffd
                out = (byte', (start, ack seq seq'{-fixme-}, False, stop{-fixme-}), sda)
                sda = if ack seq seq' then 0 else 1 -- HACK
                ack (Just 8) Nothing = True
                ack _ _ = False
                byte' = case (rdbit, seq) of (Nothing, Nothing) -> 0; (Nothing, _) -> byte; (Just b, _) -> unpack (resize (pack (byte, b)))

-- ** Tests

bsTest' = bitSlave diffd 0 (pure False)
  where diffd = sda'scl' (fromList sda') (fromList scl')
        sda' = 1:s:s:s:s:1:::::::::::::::0:::::::::::::::0:::::::::::::::1:::::::::::::::1:::::::::::::::0:::::::::::::::0:::::::::::::::1:::::::::::::::0:0:0:0:0:p:p:p: []
        --     ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^         ^^^^^^^
        scl' = 1:1:1:1:0:0:0:0:scl'
        s = 0
        p = 1

bsTest = mapM_ print (sampleN 77 bsTest')

infixr 5 :::::::::::::::
--pattern (:::::::::::::::) :: Eq a => a -> [a] -> [a]
pattern b ::::::::::::::: bs <- b : ((==b) -> True) : ((==b) -> True) : ((==b) -> True) : ((==b) -> True) : ((==b) -> True) : ((==b) -> True) : ((==b) -> True) : bs
  where b ::::::::::::::: bs = b : b : b : b : b : b : b : b : bs

-- ** Example: PCA9552

--type PCA9552 = Address 0b1100000 (ReadOne Done)

type StateX = (Unsigned 8, Vec 8 (Unsigned 8))

pca9552 :: Signal ((Bit, Bool), (Bit, Bool)) -- SDA, SCL + flanks
        -> Signal Bit                        -- SDA-out
pca9552 i = o
  where (read, conds, o) = unbundle $ bitSlave i write ack
        (ack, write) = mealyB step (0, CLaSH.Prelude.repeat 0b01010101) (read, conds)
        step :: StateX -> (Unsigned 8, (Bool, Bool, Bool, Bool)) -> (StateX, (Bool, Unsigned 8))
        step s i = (s, (True, 0)) -- FIXME
