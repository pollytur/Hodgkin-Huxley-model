module MyProject where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import Data.List
--import Data.Vector

-- custom files
import Constants 
import InputFunctions
import Biology
import Calculations

-- libraries for drawing
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Gloss

-- =================================================
-- INPUT VALUES
-- =================================================  
y = VectorLong (((vl, n_inf), (m_inf, h_inf)),
  ((gK * (n_inf^4) * (- vK), gNa* (m_inf^3)*h_inf*(- vNa ) ),
  (gL * (-vl), (gK*(n_inf^4)*(-vK))/(gNa*(m_inf^3)*h_inf*(-vNa))))) :: VectorLong Double

t = linspace tmin tmax 10000 
    
-- ==================================
-- ==================================  
  
rgTr= rungeKutta iD2 y t 0.005 

one   = Data.List.map (\a->fst(fst(fst (outOfVector a)))) rgTr
two   = Data.List.map (\a->snd(fst(fst (outOfVector a)))) rgTr
three = Data.List.map (\a->fst(snd(fst (outOfVector a)))) rgTr
four  = Data.List.map (\a->snd(snd(fst (outOfVector a)))) rgTr

five  = Data.List.map (\a->fst(fst(snd (outOfVector a)))) rgTr
six   = Data.List.map (\a->snd(fst(snd (outOfVector a)))) rgTr
seven = Data.List.map (\a->fst(snd(snd (outOfVector a)))) rgTr
eight = Data.List.map (\a->snd(snd(snd (outOfVector a)))) rgTr

 
run :: IO ()
run = (toFile def "neuron_potential.png" $ do
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