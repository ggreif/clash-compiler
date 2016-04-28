-- * A bus-slave implementation

-- Restrictions:
-- - 7bit address space
-- - missing debouncer
-- - one baud rate only

-- * Preliminaries

{-# LANGUAGE GADTSyntax, PatternSynonyms, ViewPatterns, FlexibleInstances #-}

module Slave where

import CLaSH.Prelude

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
flank :: Maybe (Unsigned 3) -> ((Bit, Bool), (Bit, Bool)) -> (Unsigned 3, Bool, Maybe Bit, Bool)
flank Nothing START = (0, True, Nothing, False)
flank Nothing STOP = (0, False, Nothing, True)
flank Nothing ACK = (0, False, Nothing, False)
flank (Just n) ((sda, _), UP) = (n+1, False, Just sda, False)
flank _ _ = (0, False, Nothing, True) -- STOP-like

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


-- * Tests

scl8 = moore up (<4) (0 :: Unsigned 3) (pure ())
  where up 7 () = 0
        up n () = n + 1


-- * The complete machine

-- Now it's time to connect the pieces.
-- The output is True: generate ack

i2c :: I2C state => state -> Signal Bit -> Signal Bit -> Signal Bool
i2c init sda scl = mealy protocol init (sda'scl' sda scl)
