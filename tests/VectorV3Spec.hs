{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module VectorV3Spec where

import Data.Vector.Unboxed.V3 (mkVectorV3, (!))
import Linear (V3 (V3))
import Prelude (Int, Double, ($), show, pure)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose, applyFun3, counterexample, (===))


spec :: Spec
spec = describe "VectorV3" $ do
  prop "should give you back what you put in" $
      \(applyFun3 -> f :: Int -> Int -> Int -> Double) -> do
    -- Pick some bounds
    nx <- choose (0, 20)
    ny <- choose (0, 20)
    nz <- choose (0, 20)
    -- And then an index within those bounds
    x <- choose (0, nx)
    y <- choose (0, ny)
    z <- choose (0, nz)

    let ix = V3 x y z
        v3 = mkVectorV3 (V3 nx ny nz) $ \(V3 a b c) -> f a b c
    pure $
      counterexample (show (V3 nx ny nz)) $
      counterexample (show (V3 x y z)) $
        -- Make sure the value we get at the index is the same one we stored
        v3 ! ix === f x y z

