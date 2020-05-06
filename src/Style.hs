module Style where

import Graphics.Gloss.Data.Color

backgroundColor :: Color
backgroundColor = makeColorI 24 73 90 255
-- backgroundColor    = makeColorI 245 245 220 1

membraneColor :: Color
membraneColor = yellow

sodium_channel :: Color
sodium_channel = makeColorI 175 73 41 200  --turquos

potassium_channel :: Color
potassium_channel = makeColorI 119 73 41 200  --green

leak_channel :: Color
leak_channel = makeColorI 240 73 41 200  --blue

sodium_ion :: Color
sodium_ion = cyan

potassium_ion :: Color
potassium_ion = makeColorI 240 73 41 255

leak_ion :: Color
leak_ion = chartreuse 
-- leak_ion           = makeColorI 24 73 42 255