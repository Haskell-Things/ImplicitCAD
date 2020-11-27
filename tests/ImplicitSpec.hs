module ImplicitSpec (spec) where

import Prelude
import Test.Hspec
import Graphics.Implicit.Test.Instances
import Graphics.Implicit
import Graphics.Implicit.Primitives
import Test.QuickCheck
import Data.Foldable
import Data.VectorSpace


spec :: Spec
spec = do
  describe "getImplicit" $ do
    for_ [ ("YZ", (1, 0, 0))
         , ("XZ", (0, 1, 0))
         , ("XY", (0, 0, 1))
         ] $ \(axis, vec) -> do
      describe ("rotation in the " <> axis <> " plane (observed by getImplicit)") $ do
        it "360 degrees is id" $
          property $
            rotate3 (vec ^* (2 * pi))
              =~= id
        it "(x + y = 360) degrees is id" $
          property $ \rads ->
            rotate3 (vec ^* (2 * pi - rads)) . rotate3 (vec ^* rads)
              =~= id

      -- NOTE(sandy): getBox is broken in HEAD, but fixed in #314
      describe ("rotation in the " <> axis <> " plane (observed by getBox)") $ do
        it "360 degrees is id" $
          expectFailure $
            getQuantizedBox . rotate3 (vec ^* (2 * pi))
              =~= getQuantizedBox
        it "(x + y = 360) degrees is id" $
          expectFailure $ \rads ->
            getQuantizedBox . rotate3 (vec ^* (2 * pi - rads)) . rotate3 (vec ^* rads)
              =~= getQuantizedBox

  describe "rotation in arbitrary planes (observed by getImplicit)" $ do
    it "360 degrees is id" $
      property $ do
        vec <- arbitrary `suchThat` (/= (0, 0, 0))
        pure $
          counterexample (show vec) $
            rotate3V (2 * pi) vec
              =~= id
    it "360 degrees is id" $
      property $ \rads -> do
        vec <- arbitrary `suchThat` (/= (0, 0, 0))
        pure $
          counterexample (show vec) $
            rotate3V (2 * pi - rads) vec . rotate3V rads vec
              =~= id


getQuantizedBox :: (Quantizable vec, Object obj vec) => obj -> (vec, vec)
getQuantizedBox = quantize epsilon . getBox

