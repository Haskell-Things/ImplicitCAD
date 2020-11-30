{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-type-defaults        #-}

module GoldenSpec.Spec (spec) where

import GoldenSpec.Util (golden)
import Graphics.Implicit
import Prelude
import Test.Hspec ( describe, Spec )

default (Int)

spec :: Spec
spec = describe "golden tests" $ do
  golden "box" 1 $
    cubeR 0 True (5, 5, 5)

  golden "example13" 1 $
    union [ rect3R 0 (0,0,0) (20,20,20)
          , translate (20,20,20) (sphere 15)
          ]

  golden "example16" 1 $
    implicit (\(x,y,z) -> x^4 + y^4 + z^4 - 15000) ((-20,-20,-20),(20,20,20))

  golden "example17" 1 $
    let
      squarePipe :: ℝ3 -> ℝ -> ℝ -> SymbolicObj3
      squarePipe (x,y,z) diameter precision =
            union
            ((\start-> translate start
               $ rect3R 0 (0,0,0) (diameter,diameter,diameter)
             )
             <$>
              zip3 (fmap (\n->(fromIntegral n/precision)*x) [0..100])
                   (fmap (\n->(fromIntegral n/precision)*y) [0..100])
                   (fmap (\n->(fromIntegral n/precision)*z) [0..100]))
     in squarePipe (10,10,10) 1 100

