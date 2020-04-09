module MyProject where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import Data.List
--import Data.Vector

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- ==================================
-- CONSTANTS
-- ==================================

-- Start and end time (in milliseconds)
tmin = 0.0
tmax = 50.0

-- Average potassium channel conductance per unit area (mS/cm^2)
gK = 36.0

-- Average sodoum channel conductance per unit area (mS/cm^2)
gNa = 120.0

-- Average leak channel conductance per unit area (mS/cm^2)
gL = 0.3

-- Membrane capacitance per unit area (uF/cm^2)
cm = 1.0

-- Potassium potential (mV)
vK = -12.0

-- Sodium potential (mV)
vNa = 115.0

-- Leak potential (mV)
vl = 10.613

-- ==================================
--  HELP FUNCTIONS DOUBLE -> DOUBLE
-- ==================================

-- Potassium ion-channel rate functions
alpha_n :: Double -> Double
alpha_n vm =(0.01 * (10.0 - vm)) / (exp(1.0 - (0.1 * vm)) - 1.0)

beta_n :: Double -> Double
beta_n vm= 0.125 * exp(-vm / 80.0)

-- Sodium ion-channel rate functions
alpha_m :: Double -> Double
alpha_m vm = (0.1 * (25.0 - vm)) / (exp(2.5 - (0.1 * vm)) - 1.0)

beta_m :: Double -> Double
beta_m vm=  4.0 * exp(-vm / 18.0)

alpha_h :: Double -> Double
alpha_h vm = 0.07 * exp(-vm / 20.0)

beta_h :: Double -> Double
beta_h vm =  1.0 / (exp(3.0 - (0.1 * vm)) + 1.0)
  
-- n, m, and h steady-state values

-- default value is 0.0
n_inf :: Double -> Double
n_inf vm =alpha_n vm / (alpha_n vm + beta_n vm)

-- default value is 0.0
m_inf :: Double -> Double
m_inf vm = alpha_m(vm) / (alpha_m(vm) + beta_m(vm))

-- default value is 0.0
h_inf :: Double -> Double
h_inf vm = alpha_h vm / (alpha_h vm + beta_h vm)
    
-- ==================================
-- ==================================

zeros :: [Int]
zeros = 0 : zeros

myTaking :: Int -> [a] -> [a] 
myTaking _ [] = []
myTaking n (x:xs)
  | n > 0 = x : myTaking (n - 1) xs 
  | otherwise = []

-- analog of np.zeros
zeroMatrix :: Int-> Int ->[[Int]]
zeroMatrix 0 _ = []
zeroMatrix x y = myTaking y zeros : zeroMatrix (x-1) y

--def Id(t):
  --  if 0.0 < t < 1.0:
    --    return 150.0
   -- elif 10.0 < t < 11.0:
   --     return 50.0
   -- return 0.0

-- t0 is assumes as time
-- Input stimulus
iD :: Double -> Double
iD t0
  |0.0  <t0 && t0< 1.0     = 150.0
  |10.0 <t0 && t0< 11.0    = 50.0
  |otherwise               = 0.0


-- applies iD function on all elements of list
idv :: [Double] -> [Double]
idv [] = []
idv (x:xs)=iD x :idv xs

-- analog of np.linspace(tmin, tmax, steps)
-- tmax is not included
linspace :: Double -> Double -> Double ->[Double] 
linspace start stop steps 
  |start ==stop =[]
  |otherwise = start : linspace (start+(stop-start)/steps) stop (steps-1)

-- from stepsize number of steps 
linspace2 ::  Double -> Double -> Int -> [Double] 
linspace2 from step n 
  | n==0 = []
  | otherwise = from : linspace2 (from+step) step (n-1)
 

-- ==================================
-- DERIVATIVES
-- ==================================

-- input list is of 4 numbers
-- vm n m h
-- prepare for odeint
computeDerivatives :: [Double] -> Double ->[Double]
computeDerivatives (vm:n:m:h) t = [dy0, dy1, dy2, dy3]
  where 
    gK0 = (gK / cm) * (n^4)
    gNa0 = (gNa / cm) * (m^3) * Data.List.head(h)
    gL0 = gL / cm
    
    dy0 = (iD t)/cm - gK0*(vm -vK) - gNa0*(vm -vNa) -gL0*(vm-vl)
     --dn/dt
    dy1 = (alpha_n vm)* (1.0 - n) - (beta_n vm) * n
    --dm/dt
    dy2 = (alpha_m vm) * (1.0 - m) - (beta_m vm) * m
    --dh/dt
    dy3 = (alpha_h vm) * (1.0 - Data.List.head(h)) - (beta_h vm) * Data.List.head(h)

-- for runge kutta
comDertypeOne:: (Double, Double) -> Double-> Double -> Double -> Double
comDertypeOne (vm, t) n m h = dy0
  where
    gK0 = (gK / cm) * (n^4)
    gNa0 = (gNa / cm) * (m^3) * h
    gL0 = gL / cm
    dy0 = (iD t)/cm - gK0*(vm -vK) - gNa0*(vm -vNa) -gL0*(vm-vl)
 
-- for runge kutta 
comDertypeTwo:: (Double, Double) ->(Double-> Double) -> (Double-> Double) -> Double
comDertypeTwo (var, vm) alpha beta = dy1
  where
  dy1 = (alpha vm)* (1.0 - var) - (beta vm) * var

-- prepare for odeint, this does not work
solveODE :: [Double] -> [Double] ->[[Double]]
solveODE _ [] = [[]]
solveODE init_values (t0:t) = 
  solution : solveODE solution t
  where
    solution = computeDerivatives init_values t0
-- ==================================

-- ==================================

y = [0.0, n_inf 0, m_inf 0, h_inf 0] :: [Double] 

t = linspace tmin tmax 10000

c = computeDerivatives y 0.0

der :: [Double]->[Double]->[[Double]]
der [] _ = []
der (t:time) vals = cur : der time cur
  where 
  cur = computeDerivatives vals t
  
dertr = der t y  

a = exp 1.9 ::Double

b = linspace2 0 0.1 20

d = [1.0, 2.0, 3.0]
-- e = fromList [1.0, 2.0, 3.0]

argFromListList :: Int ->[Double] -> Double
argFromListList n (y1:y2:y3:y4) 
  | n==1 = y1
  | n==2 = y2
  | n==3 = y3
  | otherwise = Data.List.head y4


beforeDrawing :: Int -> [Double] ->[[Double]] -> [(Double,Double)]
beforeDrawing var time results 
  = [ (x, argFromListList var y) | x <- time, y <- results]





-- def runge_kutta(x_0, y_0, h_0, X=None, n=None):
--     k_1 = my_derivate(x_0, y_0)
--     k_2 = my_derivate(x_0 + h_0 / 2, y_0 + h_0 * k_1 / 2)
--     k_3 = my_derivate(x_0 + h_0 / 2, y_0 + h_0 * k_2 / 2)
--     k_4 = my_derivate(x_0 + h_0, y_0 + h_0 * k_3)
--     d_y = (k_1 + k_4 + 2 * (k_2 + k_3)) * h_0 / 6
--     d = {'x': [x_0], 'y': [y_0], 'k_1': [k_1], 'k_2': [k_2], 'k_3': [k_3], 'k_4': [k_4], 'delta y': [d_y],
--          'method': 'runge_kutta'}
--     df = pd.DataFrame(data=d)
--     if n is None:
--         n = round((X - x_0) / h_0)
--     for i in range(n):
--         x_cur = df.iloc[i][0] + h_0
--         y_cur = df.iloc[i][1] + df.iloc[i][6]
--         k_1 = my_derivate(x_cur, y_cur)
--         k_2 = my_derivate(x_cur + h_0 / 2, y_cur + h_0 * k_1 / 2)
--         k_3 = my_derivate(x_cur + h_0 / 2, y_cur + h_0 * k_2 / 2)
--         k_4 = my_derivate(x_cur + h_0, y_cur + h_0 * k_3)
--         d_y = (k_1 + k_4 + 2 * (k_2 + k_3)) * h_0 / 6
--         d2 = {'x': [x_cur], 'y': [y_cur], 'k_1': [k_1], 'k_2': [k_2], 'k_3': [k_3], 'k_4': [k_4], 'delta y': [d_y],
--               'method': 'runge_kutta'}
--         df2 = pd.DataFrame(data=d2)
--         df = df.append(df2, ignore_index=True)
-- 

--     df.drop('k_1', axis=1, inplace=True)
--     df.drop('k_2', axis=1, inplace=True)
--     df.drop('k_3', axis=1, inplace=True)
--     df.drop('k_4', axis=1, inplace=True)
--     df.drop('delta y', axis=1, inplace=True)

--     return df

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
calNewYTypeOne:: Double -> Double -> Double -> Double -> Double -> Double -> Double
calNewYTypeOne vm t step n m h = vm +(1/6)*(k1+2*k2+2*k3+k4)*step
  where
  k1 = comDertypeOne (vm, t) n m h
  k2 = comDertypeOne (vm+step*k1/2, t+step/2) n m h
  k3 = comDertypeOne (vm+step*k1/2, t+step/2) n m h
  k4 = comDertypeOne (vm+step*k1/2, t+step) n m h
  


rungeKutta:: [Double] -> [Double] -> Double ->[[Double]]
rungeKutta _ [] _ = []
rungeKutta initVals (t0:time) step =lst:rungeKutta lst time step
  where 
  vm0 = argFromListList 1 initVals
  n0  = argFromListList 2 initVals
  m0  = argFromListList 3 initVals
  h0  = argFromListList 4 initVals
  vm  = calNewYTypeOne vm0 t0 step n0 m0 h0
  n   = calNewYTypeTwo n0 vm0 step alpha_n beta_n
  m   = calNewYTypeTwo m0 vm0 step alpha_m beta_m
  h   = calNewYTypeTwo h0 vm0 step alpha_h beta_h
  lst = [vm, n, m, h]

rgTr= rungeKutta y t 0.005 
one = Data.List.map (argFromListList 1)  rgTr
two = Data.List.map (argFromListList 2)  rgTr
three = Data.List.map (argFromListList 3)  rgTr

run :: IO ()
run = (toFile def "neuron_potential.png" $ do
    layout_title .= "Neuron potential"
    setColors [opaque blue, opaque red]
    plot (line "am" [zip t one] )) 
    <> 
    (toFile def "limit_cycles.png" $ do
    layout_title .= "Trajectories with limit cycles"
    setColors [opaque blue, opaque red]
    plot (line "Vm - n" [zip one two] )
    plot (line "Vm - m" [zip one three] )
    ) 

  --  plot (points "am points" (signal [0,7..400]))
--main :: IO()
--main = (putStrLn (show  rgTr))
--main = (putStrLn (show (one ))) <>(putStrLn (show (two ))) 
  -- <> (putStrLn (show (three )))<> (putStrLn (show (four )))