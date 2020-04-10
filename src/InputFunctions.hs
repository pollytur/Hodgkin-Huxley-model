module InputFunctions where

-- t0 is assumes as time
-- Input stimulus
iD2 :: Double -> Double
iD2 t0
  |1.0  <t0 && t0< 2.0     = 150.0
  |10.0 <t0 && t0< 11.0    = 50.0
  |otherwise               = 0.0

iD :: Double -> Double
iD t0 
	|2.0 <t0 && t0< 5.0    = 100.0
	|otherwise               =0.0

iD3 :: Double -> Double
iD3 t0 
	|3.0 <t0 && t0< 3.001    = 100.0
	|otherwise               =0.0
