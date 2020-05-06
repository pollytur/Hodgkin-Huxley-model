module Biology where

-- this module represents some biological values' changes with experimetally defined coeficients

-- n, m, h are gating variables, representing the possibility of 
-- respectively K+, Na+ and Cl- channels to be open for the corresponding ions 
-- if channel is closed, hence, the ion cannot cross the membrane

-- alpha, beta are functions with heuristically estimated coefficient. 
-- From Hodgkin-Huxley equations it is possible to define only the general type of alpha and beta functions

-- ===================================================
-- _origin are values suggested bu Hodgkin and Huxley
-- ===================================================

-- Potassium ion-channel rate functions
alpha_n_origin :: Double -> Double
alpha_n_origin vm =(0.01 * (10.0 - vm)) / (exp(1.0 - (0.1 * vm)) - 1.0)


beta_n_origin :: Double -> Double
beta_n_origin vm= 0.125 * exp(-vm / 80.0)

-- Sodium ion-channel rate functions
alpha_m_origin :: Double -> Double
alpha_m_origin vm = (0.1 * (25.0 - vm)) / (exp(2.5 - (0.1 * vm)) - 1.0)


beta_m_origin :: Double -> Double
beta_m_origin vm = 4.0 * exp(-vm / 18.0)


alpha_h_origin :: Double -> Double
alpha_h_origin vm = 0.07 * exp(-vm / 20.0)


beta_h_origin :: Double -> Double
beta_h_origin vm =  1.0 / (exp(3.0 - (0.1 * vm)) + 1.0)

-- ==========================================================================
-- values that are standard now (taken from EPFL book, referenced in README)
-- ==========================================================================

alpha_n :: Double -> Double
alpha_n vm = (0.02 * (vm - 25.0)) /(1.0- exp((25.0-vm)/9.0))

beta_n :: Double -> Double
beta_n vm= ((-0.002) * (vm - 25.0)) /(1.0- exp((vm-25.0)/9.0))

-- Sodium ion-channel rate functions
alpha_m :: Double -> Double
alpha_m vm = (0.182 * (35.0 + vm)) / (1.0 - exp((-1.0)*(35.0+vm)/9.0))

beta_m :: Double -> Double
beta_m vm=  ((-0.124) * (35.0 + vm)) / (1.0 - exp((35.0+vm)/9.0))

alpha_h :: Double -> Double
alpha_h vm = 0.25 * exp((-1.0)*(vm+90.0) / 12.0)

beta_h :: Double -> Double
beta_h vm =  0.25 * exp((vm+62.0)/ 6.0)/exp((vm+90.0)/12.0)
 

-- n_inf, m_inf, and h_inf steady-state (at infinity, when potential difference=0) 
n_inf :: Double 
n_inf = alpha_n vm / (alpha_n vm + beta_n vm) where vm = 0


m_inf :: Double 
m_inf = alpha_m(vm) / (alpha_m(vm) + beta_m(vm)) where vm = 0


h_inf :: Double 
h_inf = alpha_h vm / (alpha_h vm + beta_h vm) where vm = 0