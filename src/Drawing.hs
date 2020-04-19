module Drawing where

import Graphics.Gloss	
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import DrawingConstants
import Prelude          
import Style
import System.Random
import Constants 
import InputFunctions
import Biology

import Calculations
import GHC.Float
import System.Random
import Control.Monad

-- =================================================
-- Data Structures
-- =================================================  

--green is Na, red is K, blue is leak
data Element = Na | K |Leak deriving(Eq, Show)

data Ion = Ion {
	el     :: Element
	, x    :: Float
	, y    :: Float
	, vx   :: Float
    , vy   :: Float} deriving(Eq, Show)

data World = World { 
	ions 		 :: [Ion] ,
	membrane :: [Float],
  hole     :: [Float],
  holeCol  :: Color,
	state 	 :: VectorLong Double ,
	time		 :: Double
}

-- toIon :: Element->Float ->Float -> Ion
-- toIon element x0 y0 = Ion{el=element, x=x0, y=y0, vx=0, vy=0}	

-- =================================================
-- Initial State (make it a separate file further)
-- ================================================= 

initialState = VectorLong (((vl, n_inf), (m_inf, h_inf)),
  ((gK * (n_inf^4) * (- vK), gNa* (m_inf^3)*h_inf*(- vNa ) ),
  (gL * (-vl), (gK*(n_inf^4)*(-vK))/(gNa*(m_inf^3)*h_inf*(-vNa))))) :: VectorLong Double

-- =================================================
-- Update Functions
-- ================================================= 

updateFunc :: Graphics.Gloss.Data.ViewPort.ViewPort-> Float -> World -> World
updateFunc _ dt (World {ions = ion, membrane = mem, hole=h,holeCol =hc, state = st, time = t}) = 
  World {ions = ionNew, membrane = mem, hole=h,holeCol = col, state = newState,time =(t+(GHC.Float.float2Double dt)) }
  where
    newState = oneStep iD2 st (t+(GHC.Float.float2Double dt)) (GHC.Float.float2Double dt)
    curK   = normalize (fst(fst(snd (outOfVector newState))))
    curNa  = normalize (snd(fst(snd (outOfVector newState))))
    curL   = normalize (fst(snd(snd (outOfVector newState))))
    gatN   = round (fst(fst(snd (outOfVector newState)))*255) :: Int
    gatM   = round (snd(fst(snd (outOfVector newState)))*255) :: Int
    gatH   = round (fst(snd(snd (outOfVector newState)))*255) :: Int
    col    = makeColorI gatN gatM gatH 255
    ionNew = map (\i->(updateIon i dt curK curNa curL)) ion


normalize :: Double -> Float
normalize a 
	| (abs a) >= 0.1 = GHC.Float.double2Float a
	|otherwise = 0.0


updateVelocity :: Element -> Float -> Float ->Float -> Float
updateVelocity el curK curNa curL
	|el == Na = curNa*2
	|el == K  = curK*2
	|otherwise = curL*2

-- to do add y velocity to go to holes in membrane
updateIon :: Ion -> Float  -> Float -> Float -> Float ->  Ion
updateIon Ion{el=elem, x=x0, y=y0, vx = vx0, vy = vy0} dt curK curNa curL =
	Ion{el=elem, x=x1, y=y1, vx = vx1, vy = vy1}
	where
		x1  = x0 + vx0*dt
		y1  = y0 + vy0*dt
		vx1 = updateVelocity elem curK curNa curL 
		vy1 = 0 

-- =================================================
-- Generate the World
-- =================================================  

-- bound should be positive
membraneParts :: Float -> Float -> [Float]
membraneParts curr bound 
	| curr < (-1)*bound = []
	| otherwise = curr : membraneParts (curr - membrane_distance - membrane_height) bound


holes :: [Float]-> [Float]    
holes lst = map (\el -> ((fst el)+(snd el))/2) combined
  where
    combined = zip lst (tail lst)    


randomIon :: Bool -> Element -> IO Ion
randomIon bol element = do{ x <- randomRIO (a, b);
                y <- randomRIO (-field_bound, field_bound);
                return $ Ion { el = element, x = x,y= y, vx=0,vy=0} }
                where
                a = case bol of 
                   True ->  -field_bound; 
                   False -> 0
                b = case bol of 
                   True ->  0; 
                   False -> field_bound
                
   
randomState :: IO World
randomState = do {kin   <- (replicateM number_of_potassium_in  (randomIon True  K)) 
                   +++ (replicateM number_of_potassium_out (randomIon False K)) 
                   +++ (replicateM number_of_sodium_in (randomIon True  Na)) 
                   +++ (replicateM number_of_sodium_out (randomIon False Na ))
                   +++ (replicateM number_of_leak_in  (randomIon True Leak ))
                   +++ (replicateM number_of_leak_out (randomIon False Leak ))  ;
                  return $ emptyState { ions = kin }}                    
                 

emptyState :: World
emptyState = World { ions = [],
                   membrane = (membraneParts field_bound field_bound), 
                   state = initialState, 
                   hole = (holes (membraneParts field_bound field_bound)),
                   holeCol = makeColorI 0 0 0 0, 
                   time = 0.0
                   }  


-- =================================================
-- Rendering
-- =================================================  

drawWorld :: World -> Picture
drawWorld World{ions = ion, membrane =mem, hole = h, holeCol =c,  state =st, time=t} = 
  pictures [hol, membraneDrawn, ionsDrawn, labels]
	where
        ionsDrawn = drawIons ion
        membraneDrawn = drawMembrane mem
        labels = pictures [labelOne, labelTwo]
        hol = drawInHoles h c


labelOne :: Picture
labelOne = Color white
        $ Translate (-400) 450
        $ rectangleSolid 150 50 
        <> 
        (Color black 
        $ Translate (-65) (-10) 
        $ scale 0.2 0.2 (text inside))
  where 
    inside  = "Inside Cell"


labelTwo :: Picture
labelTwo = Color white
        $ Translate (400) 450
        $ rectangleSolid 150 50 
        <>
        (Color black 
        $ Translate (-72) (-10) 
        $ scale 0.2 0.2 (text outside))
    where
      outside = "Outside Cell"


drawIons :: [Ion] -> Picture
drawIons ions = pictures lst
	where
		lst = map drawIon ions


drawIon :: Ion -> Picture 
drawIon Ion{el=e, x=x0, y=y0} 
	|e==Na   = drawBall x0 y0 sodium_ion "Na+"
	|e==K    = drawBall x0 y0 potassium_ion "K+"
	|e==Leak = drawBall x0 y0 leak_ion "Cl-"


drawBall :: Float -> Float ->Color -> String -> Picture
drawBall  delta_x delta_y col str
        = Color col
        $ Translate delta_x delta_y
        $ circleSolid ion_radius 
        <> (Color black 
          $ Translate (-ion_radius/2) (-ion_radius/2)
          $ scale 0.1 0.1 (text str ))      


drawMembrane :: [Float] -> Picture
drawMembrane lst = pictures (map drawMembranePart lst)


drawInHoles:: [Float] -> Color ->Picture
drawInHoles lst c = pictures (map (drawHole c) lst)

drawHole :: Color -> Float -> Picture
drawHole c delta_y 
        = Color c
        $ Translate 0 delta_y
        $ rectangleSolid membrane_width (membrane_distance - membrane_height)  

drawMembranePart :: Float-> Picture
drawMembranePart  delta_y
        = Color membraneColor
        $ Translate 0 delta_y
        $ rectangleSolid membrane_width membrane_height

-- =================================================
-- Help Functions
-- ================================================= 
-- distance :: Point -> Point -> Float
-- distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
--     where dx = x2 - x1
--           dy = y2 - y1

(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2 





