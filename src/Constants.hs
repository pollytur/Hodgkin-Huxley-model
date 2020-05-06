module Constants where
-- ==================================
-- CONSTANTS
-- _original are from the original Hodgkin Huxley experiment
-- ==================================

-- Start and end time (in milliseconds)
tmin = 0.0           ::Double
tmax = 80.0          ::Double

-- Average potassium channel conductance per unit area (mS/cm^2)
gK_original = 36.0   ::Double
gK = 35.0            ::Double

-- Average sodoum channel conductance per unit area (mS/cm^2)
gNa_original = 120.0 ::Double
gNa = 40.0           ::Double

-- Average leak channel conductance per unit area (mS/cm^2)
gL_original = 0.3    ::Double
gL = 0.3             ::Double
-- Membrane capacitance per unit area (uF/cm^2)
cm_original = 1.0    ::Double
cm = 1.0             ::Double

-- Same as E_x in the EPFL book

-- Potassium potential (mV)
vK_original = -12.0  ::Double
vK = -77.0           ::Double

-- Sodium potential (mV)
vNa_original = 115.0 ::Double
vNa = 55.0           ::Double

-- Leak potential (mV)
vl_original = 10.613 ::Double
vl = - 65.0          ::Double