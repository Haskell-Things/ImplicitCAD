{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ImplicitSpec (spec) where

import Prelude (mempty, (*), (<>), (-), (/=), ($), (.), pi, id)
import Test.Hspec ( describe, it, Spec )
import Graphics.Implicit.Test.Instances
    ( (=~=), Quantizable(quantize), epsilon )
import Graphics.Implicit
import Graphics.Implicit.Primitives ( Object(getBox) )
import Test.QuickCheck
    ( Arbitrary(arbitrary),
      suchThat,
      expectFailure,
      forAll,
      Testable(property) )
import Data.Foldable ( for_ )
import Data.VectorSpace ( (^*) )
import Test.Hspec.QuickCheck (prop)
import Graphics.Implicit.Definitions


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
        forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
          rotate3V (2 * pi) vec
            =~= id
    it "(x + y = 360) degrees is id" $
      property $ \rads -> do
        forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
          rotate3V (2 * pi - rads) vec . rotate3V rads vec
            =~= id

  describe "rotation in arbitrary planes (observed by getBox)" $ do
    it "360 degrees is id" $
      expectFailure $ do
        forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
          getQuantizedBox . rotate3V (2 * pi) vec
            =~= getQuantizedBox
    it "(x + y = 360) degrees is id" $
      expectFailure $ \rads -> do
        forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
          getQuantizedBox . rotate3V (2 * pi - rads) vec . rotate3V rads vec
            =~= getQuantizedBox

  monoidSpec

monoidSpec :: Spec
monoidSpec = do
  describe "monoid laws (getImplicit)" $ do
    prop "a <> mempty = a" $ \obj ->
      obj =~= obj <> mempty @SymbolicObj3
    prop "mempty <> a = a" $ \obj ->
      obj =~= mempty @SymbolicObj3 <> obj
    prop "(a <> b) <> c = a <> (b <> c)" $ \a b (c :: SymbolicObj3) ->
      (a <> b) <> c =~= a <> (b <> c)

  describe "union (getImplicit)" $
    prop "union [a] = a" $ \obj ->
      union @SymbolicObj3 [obj] =~= obj

  describe "mempty stays mempty (getImplicit)" $ do
    -- prop "rect 0 = mempty" $
      -- rect3R 0 0 0 =~= mempty
    prop "rotate xyz mempty = mempty" $ \xyz ->
      rotate3 xyz mempty =~= mempty
    prop "translate xyz mempty = mempty" $ \xyz ->
      translate @SymbolicObj3 xyz mempty =~= mempty







getQuantizedBox :: (Quantizable vec, Object obj vec) => obj -> (vec, vec)
getQuantizedBox = quantize epsilon . getBox

