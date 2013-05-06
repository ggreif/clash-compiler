{-# LANGUAGE DataKinds #-}
module FIR where

import CLaSH.Prelude

dotp as bs = vfoldl (+) 0 (vzipWith (*) as bs)

fir coeffs x_t = y_t
  where
    y_t = dotp coeffs xs
    xs  = window x_t

topEntity :: Sync (Signed 16) -> Sync (Signed 16)
topEntity = fir (0 :> 1 :> 2 :> 3 :> Nil)
