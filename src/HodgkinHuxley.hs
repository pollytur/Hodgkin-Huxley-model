module HodgkinHuxley where
import Graphics.Gloss
import DrawingConstants
import Drawing (drawWorld, updateFunc, initialState, randomState)
import Prelude         
import Style
import Data.List
import Calculations
-- libraries for drawing
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Constants 
import InputFunctions


run:: IO ()
run = do {initialModel <- randomState;
    graphs;
    simulate displayWindow backgroundColor simulationRate initialModel drawingFunc updateFunc;
    }
    where
      displayWindow = (InWindow "Hodgkin-Huxley-Simulation" (1200, 1200) (20, 20));  drawingFunc = drawWorld


time :: [Double]
time = linspace tmin tmax 10000 
    
-- ==================================
-- ==================================  

--  rgTr= rungeKutta iD2 y t 0.005  ((tmax - tmin)/10000)
rgTr :: [VectorLong Double]
rgTr = rungeKutta iD2 initialState time 0.005 

-- ==============================================
-- EXTRACTION OF THE RESULTS AFTER RUNGE-KUTTA
-- ==============================================  
-- n, m, h are gating variables, representing the possibility of 
-- respectively K+, Na+ and Cl- channels to be open for the corresponding ions 
-- if channel is closed, hence, the ion cannot cross the membrane
    
neuron_potential :: [Double]
neuron_potential   = Data.List.map (\a->fst(fst(fst (outOfVector a)))) rgTr :: [Double]

n :: [Double]
n   = Data.List.map (\a->snd(fst(fst (outOfVector a)))) rgTr

m :: [Double]
m = Data.List.map (\a->fst(snd(fst (outOfVector a)))) rgTr

h :: [Double]
h  = Data.List.map (\a->snd(snd(fst (outOfVector a)))) rgTr

kPlus :: [Double]
kPlus  = Data.List.map (\a->fst(fst(snd (outOfVector a)))) rgTr

naPlus :: [Double]
naPlus   = Data.List.map (\a->snd(fst(snd (outOfVector a)))) rgTr

leak :: [Double]
leak = Data.List.map (\a->fst(snd(snd (outOfVector a)))) rgTr

kVSna :: [Double]
kVSna = Data.List.map (\a->snd(snd(snd (outOfVector a)))) rgTr

graphs :: IO ()     
graphs = (toFile def "neuron_potential.png" $ do
    layout_title .= "Neuron potential"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, opaque Graphics.Rendering.Chart.Easy.red]
    plot (Graphics.Rendering.Chart.Easy.line "Neuron potential" [Data.List.zip time neuron_potential] )) 
    <> 
    (toFile def "gating_variables_dynamics.png" $ do
    layout_title .= "Gating Variables Dynamics"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, 
      opaque Graphics.Rendering.Chart.Easy.red,
      opaque Graphics.Rendering.Chart.Easy.green]
    plot (Graphics.Rendering.Chart.Easy.line "Vm - n" [Data.List.zip time n] )
    plot (Graphics.Rendering.Chart.Easy.line "Vm - m" [Data.List.zip time m])
    plot (Graphics.Rendering.Chart.Easy.line "Vm - h" [Data.List.zip time h])
    ) 
    <>
    (toFile def "currents.png" $ do
    layout_title .= "Currents vs Time"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, 
      opaque Graphics.Rendering.Chart.Easy.red,
      opaque Graphics.Rendering.Chart.Easy.green]
    plot (Graphics.Rendering.Chart.Easy.line "K+" [Data.List.zip time kPlus] )
    plot (Graphics.Rendering.Chart.Easy.line "Na+" [Data.List.zip time naPlus])
    plot (Graphics.Rendering.Chart.Easy.line "Leak" [Data.List.zip time leak])
    ) 
    <>
    (toFile def "currents_ratio.png" $ do
    layout_title .= "K vs NA currents"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, opaque Graphics.Rendering.Chart.Easy.red]
    plot (Graphics.Rendering.Chart.Easy.line "I_k/I_na" [Data.List.zip time kVSna] )) 










