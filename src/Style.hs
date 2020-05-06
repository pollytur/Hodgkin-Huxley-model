module Style where

import Graphics.Gloss.Data.Color

backgroundColor    = makeColorI 24 73 90 255
-- backgroundColor    = makeColorI 245 245 220 1

membraneColor      = yellow
sodium_channel     = makeColorI 175 73 41 200  --turquos
potassium_channel  = makeColorI 119 73 41 200  --green
leak_channel       = makeColorI 240 73 41 200  --blue

sodium_ion         = cyan
potassium_ion      = makeColorI 240 73 41 255
leak_ion           = chartreuse 
-- leak_ion           = makeColorI 24 73 42 255