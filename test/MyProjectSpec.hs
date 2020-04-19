module MyProjectSpec where

import Test.Hspec
import Test.QuickCheck
import InputFunctions
import Biology
import Calculations
import Drawing
import Data.List
import Constants (vl)

vec = VectorLong(((1, 2),(3, 4)), ((5, 6),(7, 8)))
vecList = [vec, vec, vec, vec]
lstRes = [1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,10.5,11.0]
short = VectorShort (vl, n_inf, m_inf, h_inf) :: VectorShort Double
outOfStepOne = (((-65.84811050861654,0.37649168633276453),(0.6060682233232992,3.145228522480321e-3)),((7.842202706723386,-3.3846721174496497),(-0.25443315258496285,-2.316975598993171)))



spec :: Spec
spec = do
-- test :: IO ()
-- test = hspec $ do	
  describe "Tests for MyProject module" $ do

 -- =============================
 --  tests for input functions
 -- ============================= 	
    it "iD2 0 = 0" $ do
      iD2 0 `shouldBe` 0.0

    it "iD2 1.5 = 150" $ do
      iD2 1.5 `shouldBe` 150.0  

    it "iD2 5.5 = 0" $ do
      iD2 5.5 `shouldBe` 0.0 

    it "iD2 10.5 = 50" $ do
      iD2 10.5 `shouldBe` 50.0  

    it "iD2 11.5 = 0" $ do
      iD2 11.5 `shouldBe` 0.0  

 -- =============================
 --  tests for biology
 -- =============================     

    it "alpha_n_origin 0 " $ do
      alpha_n_origin 0.0 `shouldBe` 5.819767068693265e-2

    it "beta_n_origin 0 " $ do
      beta_n_origin 0.0 `shouldBe` 0.125

    it "alpha_m_origin 0 " $ do
      alpha_m_origin 0.0 `shouldBe` 0.22356372458463003

    it "beta_m_origin 0 " $ do
      beta_m_origin 0.0 `shouldBe` 4.0

    it "alpha_h_origin 0 " $ do
      alpha_h_origin 0.0 `shouldBe` 7.0e-2

    it "beta_h_origin 0 " $ do
      beta_h_origin 0.0 `shouldBe` 4.742587317756678e-2

    it "alpha_n 0 " $ do
      alpha_n 0.0 `shouldBe` 3.314937491689673e-2

    it "beta_n 0 " $ do
      beta_n 0.0 `shouldBe` 5.331493749168968e-2

    it "alpha_m 0 " $ do
      alpha_m 0.0 `shouldBe` 6.503106067365285

    it "beta_m 0 " $ do
      beta_m 0.0 `shouldBe` 9.068765029283131e-2

    it "alpha_h 0 " $ do
      alpha_h 0.0 `shouldBe` 1.382710925369584e-4

    it "beta_h 0 " $ do
      beta_h 0.0 `shouldBe` 4.250509985023506

    it "n_inf " $ do
      n_inf  `shouldBe` 0.38338794345867955

    it "m_inf " $ do
      m_inf  `shouldBe` 0.9862465138923029

    it "h_inf " $ do
      h_inf  `shouldBe` 3.252941297554017e-5

 -- =============================
 --  tests for Calculations
 -- ============================= 
    it "outOfVector" $ do
      outOfVector vec `shouldBe` (((1, 2),(3, 4)), ((5, 6),(7, 8))) 

    it "projection of first element " $ do
      Data.List.map (\a->fst(fst(fst (outOfVector a)))) vecList `shouldBe` [1,1,1,1] 

    it "projection of second element " $ do
      Data.List.map (\a->snd(fst(fst (outOfVector a)))) vecList `shouldBe` [2,2,2,2]  

    it "projection of thirs element " $ do
      Data.List.map (\a->fst(snd(fst (outOfVector a)))) vecList `shouldBe` [3,3,3,3]

    it "projection of forth element " $ do
      Data.List.map (\a->snd(snd(fst (outOfVector a)))) vecList `shouldBe` [4,4,4,4]
      
    it "projection of fifth element " $ do
      Data.List.map (\a->fst(fst(snd (outOfVector a)))) vecList `shouldBe` [5,5,5,5]

    it "projection of sixth element " $ do
      Data.List.map (\a->snd(fst(snd (outOfVector a)))) vecList `shouldBe` [6,6,6,6] 

    it "projection of seventh element " $ do
      Data.List.map (\a->fst(snd(snd (outOfVector a)))) vecList `shouldBe` [7,7,7,7] 

    it "projection of eights element " $ do
      Data.List.map (\a->snd(snd(snd (outOfVector a)))) vecList `shouldBe` [8,8,8,8]

    it "comDertypeOne" $ do
     	comDertypeOne iD2 short 10.5 `shouldBe` 41.075696647311396
      
    it "linspace" $ do
      linspace 1 11 20 `shouldBe` lstRes

    it "comDertypeTwo" $ do
     	comDertypeTwo (n_inf, vl) alpha_n beta_n `shouldBe` (-6.89625712591499e-2)

    it "calNewYTypeOne" $ do
     	calNewYTypeOne iD2 short 0.0 0.1 `shouldBe` (-65.84811050861654)

    it "calNewYTypeTwo" $ do
     	calNewYTypeTwo n_inf vl 0.1 alpha_n beta_n `shouldBe` 0.37649168633276453

    it "oneStep from Runge Kutta" $ do
     	outOfVector (oneStep iD2 initialState 0.0 0.1) `shouldBe` outOfStepOne

 -- =============================
 --  tests for Drawing
 -- =============================  

    it "normalize 0.05 " $ do
      normalize 0.05 `shouldBe` 0.0

    it "normalize 5.0 " $ do
      normalize 5.0 `shouldBe` 5.0 

    it "normalize -0.05 " $ do
      normalize (-0.05) `shouldBe` 0.0

    it "normalize -5.0 " $ do
      normalize (-5.0) `shouldBe` (-5.0)     

    it "update velocity " $ do
      updateVelocity Na 1.0 2.0 3.0 `shouldBe` 4.0

    it "update velocity " $ do
      updateVelocity K 1.0 2.0 3.0 `shouldBe`  2.0

    it "update velocity " $ do
      updateVelocity Leak 1.0 2.0 3.0  `shouldBe` 6.0

    it "update ion" $ do
      updateIon Ion{el = Na, x=0.0, y=3.5, vx=2.8, vy=0.0} 0.1 1.0 2.0 3.0 `shouldBe` Ion{el = Na, x=0.28, y=3.5, vx=4.0, vy=0.0} 











