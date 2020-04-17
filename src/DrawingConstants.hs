module DrawingConstants where


-- https://ru.wikipedia.org/wiki/Потенциал_покоя
-- Снаружи клетки всегда больше ионов натрия и меньше калия
-- Ионы	Концентрация в саркоплазме (ммоль/л)	Концентрация вне клетки (ммоль/л)
-- K+	       140	                                  2,5
-- Na+	        10	                                  120
-- Cl-	        3-4	                                  120
-- from above ratio in absolute numbers 
-- in:  K:Na:Cl = 70:5:2
-- out: K:Na:Cl = 1:48:48
number_of_potassium_in  = 70  :: Int
number_of_leak_in       = 5   :: Int
number_of_sodium_in	    = 2   :: Int

number_of_potassium_out  = 1  :: Int
number_of_leak_out       = 48 :: Int
number_of_sodium_out	 = 48 :: Int

worldSize = 1200.0
separationDistance = 10.0
ion_radius         = 15.0  :: Float
membrane_width     = 50.0  :: Float
membrane_height    = 100.0 :: Float
membrane_distance  = 30.0  :: Float

simulationRate = 100        :: Int
shift = 20.0			   :: Float

-- is int?
field_bound = 500.0        :: Float 