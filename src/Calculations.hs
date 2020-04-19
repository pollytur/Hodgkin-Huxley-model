module Calculations where

import Data.List
--import Data.Vector

-- custom files
import Constants 
import InputFunctions
import Biology

data VectorLong a= VectorLong(((a, a),(a, a)), ((a, a),(a, a)))
  deriving (Show)
data VectorShort a= VectorShort (a, a,a, a)


outOfVector:: VectorLong a -> (((a, a),(a, a)), ((a, a),(a, a)))
outOfVector (VectorLong vec) = vec

-- analog of np.linspace(tmin, tmax, steps)
-- tmax is not included
linspace :: Double -> Double -> Double ->[Double] 
linspace start stop steps 
  |start ==stop =[stop]
  |otherwise = start : linspace (start+(stop-start)/steps) stop (steps-1)
  

comDertypeOne:: (Double->Double)->VectorShort Double -> Double-> Double
comDertypeOne inputf (VectorShort(vm, n, m, h)) t = dy0
  where
    gK0 = (gK / cm) * (n^4)
    gNa0 = (gNa / cm) * (m^3) * h
    gL0 = gL / cm
    dy0 = (inputf t)/cm - gK0*(vm -vK) - gNa0*(vm -vNa) -gL0*(vm-vl)
 
-- for runge kutta 
comDertypeTwo:: (Double, Double) ->(Double-> Double) -> (Double-> Double) -> Double
comDertypeTwo (var, vm) alpha beta = dy1
  where
  dy1 = (alpha vm)* (1.0 - var) - (beta vm) * var

-- vm is x in this case
-- var is like y
-- returns value of var for the following step
calNewYTypeTwo::Double -> Double -> Double->(Double->Double)->(Double->Double)->Double
calNewYTypeTwo var vm step alpha beta = var+k1*step
  where
  k1 = comDertypeTwo (var, vm)                  alpha beta
  --k2 = comDertypeTwo (var+step*k1/2, vm+step/2) alpha beta
 -- k3 = comDertypeTwo (var+step*k2/2, vm+step/2) alpha beta
 -- k4 = comDertypeTwo (var+step*k3, vm+step)     alpha beta


-- vm is y in this case
-- t is like x
-- returns value of vm for the following step
calNewYTypeOne:: (Double->Double)->VectorShort Double -> Double -> Double  -> Double
calNewYTypeOne inputf (VectorShort (vm,n,m, h)) t step = vm +(1/6)*(k1+2*k2+2*k3+k4)*step
  where
  k1 = comDertypeOne inputf (VectorShort (vm,n,m, h)) t
  k2 = comDertypeOne inputf (VectorShort (vm+step*k1/2,n,m, h)) t+step/2
  k3 = comDertypeOne inputf (VectorShort (vm+step*k1/2,n,m, h)) t+step/2 
  k4 = comDertypeOne inputf (VectorShort (vm+step*k1/2,n,m, h)) t+step
  

rungeKutta:: (Double->Double)->VectorLong Double -> [Double] -> Double ->[VectorLong  Double]
rungeKutta _ _ [] _ = []
rungeKutta inputf (VectorLong(((vm0,n0),(m0, h0)), a)) (t0:time) step 
  = lst:rungeKutta inputf lst time step
  where 
  vm  = calNewYTypeOne inputf (VectorShort(vm0, n0, m0, h0)) t0 step
  n   = calNewYTypeTwo n0 vm0 step alpha_n beta_n
  m   = calNewYTypeTwo m0 vm0 step alpha_m beta_m
  h   = calNewYTypeTwo h0 vm0 step alpha_h beta_h
  gK0  = gK * (n^4) * (vm - vK )
  gNa0 = gNa* (m^3) * h *  (vm - vNa )
  gL0  = gL * (vm - vl)
  
  lst = VectorLong (((vm, n), (m, h)),((gK0, gNa0),(gL0, gK0/gNa0)))


-- defineModel:: (Double->Double)->Double->(VectorLong Double->[Double]->[VectorLong  Double])
-- defineModel inputf step = rungeKutta inputf

oneStep :: (Double->Double)->VectorLong Double -> Double -> Double ->VectorLong  Double
oneStep inputf vect t0 step = head res
  where
    res = rungeKutta inputf vect [t0] step


