module MyProject where
import Graphics.Gloss
import DrawingConstants
import Drawing
-- run = display (InWindow "Nice Window" (1200, 1200) (200, 200)) blue (Translate (-200) (-200) ((Circle 20)))  
-- import Graphics.Gloss
import Prelude          hiding ( lines )
import Style

import Data.List
import Calculations
-- libraries for drawing
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
-- import Graphics.Gloss

import Constants 
import InputFunctions
-- import Biology


run:: IO ()
run = do {initialModel <- randomState;
    graphs;
    simulate displayWindow backgroundColor simulationRate initialModel drawingFunc updateFunc;
    }
    where
      displayWindow = (InWindow "Hodgkin-Huxley-Simulation" (1200, 1200) (20, 20));  drawingFunc = drawWorld



t = linspace tmin tmax 10000 
    
-- ==================================
-- ==================================  

--  rgTr= rungeKutta iD2 y t 0.005  ((tmax - tmin)/10000)
rgTr= rungeKutta iD2 initialState t 0.005 

one   = Data.List.map (\a->fst(fst(fst (outOfVector a)))) rgTr
two   = Data.List.map (\a->snd(fst(fst (outOfVector a)))) rgTr
three = Data.List.map (\a->fst(snd(fst (outOfVector a)))) rgTr
four  = Data.List.map (\a->snd(snd(fst (outOfVector a)))) rgTr

five  = Data.List.map (\a->fst(fst(snd (outOfVector a)))) rgTr
six   = Data.List.map (\a->snd(fst(snd (outOfVector a)))) rgTr
seven = Data.List.map (\a->fst(snd(snd (outOfVector a)))) rgTr
eight = Data.List.map (\a->snd(snd(snd (outOfVector a)))) rgTr

graphs :: IO ()     
graphs = (toFile def "neuron_potential.png" $ do
    layout_title .= "Neuron potential"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, opaque Graphics.Rendering.Chart.Easy.red]
    plot (Graphics.Rendering.Chart.Easy.line "am" [Data.List.zip t one] )) 
    <> 
    (toFile def "gating_variables_dynamics.png" $ do
    layout_title .= "Gating Variables Dynamics"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, 
      opaque Graphics.Rendering.Chart.Easy.red,
      opaque Graphics.Rendering.Chart.Easy.green]
    plot (Graphics.Rendering.Chart.Easy.line "Vm - n" [Data.List.zip t two] )
    plot (Graphics.Rendering.Chart.Easy.line "Vm - m" [Data.List.zip t three])
    plot (Graphics.Rendering.Chart.Easy.line "Vm - h" [Data.List.zip t four])
    ) 
    <>
    (toFile def "currents.png" $ do
    layout_title .= "Currents vs Time"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, 
      opaque Graphics.Rendering.Chart.Easy.red,
      opaque Graphics.Rendering.Chart.Easy.green]
    plot (Graphics.Rendering.Chart.Easy.line "Vm - n" [Data.List.zip t five] )
    plot (Graphics.Rendering.Chart.Easy.line "Vm - m" [Data.List.zip t six])
    plot (Graphics.Rendering.Chart.Easy.line "Vm - h" [Data.List.zip t seven])
    ) 
    <>
    (toFile def "currents_ratio.png" $ do
    layout_title .= "K vs NA currents"
    setColors [opaque Graphics.Rendering.Chart.Easy.blue, opaque Graphics.Rendering.Chart.Easy.red]
    plot (Graphics.Rendering.Chart.Easy.line "I_k/I_na" [Data.List.zip t eight] )) 










