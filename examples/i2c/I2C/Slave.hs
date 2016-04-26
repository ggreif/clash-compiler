-- * A bus-slave implementation

-- Restrictions:
-- - 7bit address space
-- - missing debouncer
-- - one baud rate only

-- * Preliminaries

{-# LANGUAGE GADTSyntax, PatternSynonyms, ViewPatterns #-}

module Slave where

import CLaSH.Prelude

-- * Types

data State where
  Start :: State
  Collect :: Unsigned 3 -> Unsigned 8 -> State
  Stop :: Unsigned 8 -> State

-- * Flank detection

-- ** Could this go into the prelude?

pattern I <- ((==high) -> True)
  where I = high
pattern O <- ((==low) -> True)
  where O = low

-- ** Level and derivative

pattern HIGH = (I, False)
pattern LOW = (O, False)
pattern UP = (I, True)
pattern DOWN = (O, True)

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

-- * Transfer functions for derivatives

derive :: Eq a => a -> a -> (a, (a, Bool))
derive a0 a = (a, (a, a /= a0))

-- * Isolate flanks of the two wires

sda'scl' :: Signal Bit -> Signal Bit -> Signal ((Bit, Bool), (Bit, Bool))
sda'scl' sda scl = bundle (sda', scl')
  where sda' = mealy derive O sda
        scl' = mealy derive O scl

-- * Tests

scl8 = moore up (<4) (0 :: Unsigned 3) (pure ())
  where up 7 () = 0
        up n () = n + 1
