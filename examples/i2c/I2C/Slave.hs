-- * A bus-slave implementation

-- Restrictions:
-- - 7bit address space
-- - missing debouncer


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

pattern I <- ((==high) -> True)
  where I = high
pattern O <- ((==low) -> True)
  where O = low

pattern HIGH = (I, False)
pattern LOW = (O, False)
pattern UP = (I, True)
pattern DOWN = (O, True)

pattern START = (DOWN, HIGH)
pattern STOP = (UP, HIGH)

--                       +-- SDA      +--- SCL
--                       |    +- SDA' |    +- SCL'
--                       v    v       v    v
flank :: Unsigned 3 -> ((Bit, Bool), (Bit, Bool)) -> (Unsigned 3, Bool, Maybe Bit, Bool)
flank 7 ((sda, sda'), (scl, scl')) = (0, True, Nothing, False)
