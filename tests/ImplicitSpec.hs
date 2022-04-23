{- ORMOLU_DISABLE -}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ImplicitSpec (spec) where

import Prelude (Fractional, not, fmap, pure, negate, (+), String,  Show, Monoid, mempty, (*), (/), (<>), (-), (/=), ($), (.), pi, id)
import Test.Hspec (xit, SpecWith, describe, Spec)
import Graphics.Implicit
    ( difference,
      rotate,
      transform,
      rotate3,
      rotate3V,
      transform3,
      union,
      SymbolicObj2,
      SymbolicObj3,
      scale,
      emptySpace,
      fullSpace,
      complement,
      differenceR,
      translate,
      withRounding,
      Object )
import Graphics.Implicit.Primitives (rotateQ)
import Test.QuickCheck
    (Testable, property, expectFailure,  Arbitrary(arbitrary),
      suchThat,
      forAll)
import Data.Foldable ( for_ )
import Test.Hspec.QuickCheck (prop)
import Linear (V2(V2), V3(V3), V4(V4), (^*) , Epsilon(nearZero))
import qualified Linear
import Graphics.Implicit (unionR)
import Graphics.Implicit (intersectR)
import Graphics.Implicit (extrude)
import Graphics.Implicit (cylinder2)
import Graphics.Implicit (mirror)
import Graphics.Implicit.Test.Instances (Observe, (=~=))

------------------------------------------------------------------------------
-- Tests showing equivalencies between algebraic formulations of symbolic
-- objects, in both 2d and 3d. Equality is observational, based on random
-- sampling of the underlying 'getImplicit' function.
spec :: Spec
spec = do
  describe "symbolic obj 2" $ do
    idempotenceSpec  @SymbolicObj2
    identitySpec     @SymbolicObj2
    homomorphismSpec @SymbolicObj2
    monoidSpec       @SymbolicObj2
    inverseSpec      @SymbolicObj2
    annihilationSpec @SymbolicObj2
    rotation2dSpec
    transform2dSpec

  describe "symbolic obj 3" $ do
    idempotenceSpec  @SymbolicObj3
    identitySpec     @SymbolicObj3
    homomorphismSpec @SymbolicObj3
    monoidSpec       @SymbolicObj3
    inverseSpec      @SymbolicObj3
    annihilationSpec @SymbolicObj3
    rotation3dSpec
    transform3dSpec
    misc3dSpec

------------------------------------------------------------------------------
-- All the constraints we need in scope to parameterize tests by both 2d and
-- 3d symbolic objects.
type TestInfrastructure obj vec test outcome =
  ( Object obj vec
  , Observe test outcome obj
  , Monoid obj
  , Show outcome
  , Show test
  , Show obj
  , Show vec
  , Arbitrary obj
  , Arbitrary vec
  , Epsilon vec
  , Fractional vec
  )

------------------------------------------------------------------------------
-- Tests proving that symbolic objects form a monoid.
monoidSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
monoidSpec = describe "monoid laws" $ do
  prop "a <> mempty = a" $ \obj ->
    obj =~= obj <> mempty @obj

  prop "mempty <> a = a" $ \obj ->
    obj =~= mempty @obj <> obj

  prop "(a <> b) <> c = a <> (b <> c)" $ \a b (c :: obj) ->
    (a <> b) <> c =~= a <> (b <> c)

------------------------------------------------------------------------------
-- Tests showing that 'translate' is a no-op for both 'emptySpace' and
-- 'fullSpace'. Additionally, that 'scale' is a no-op on 'emptySpace' (but not
-- for 'fullSpace', because scaling by 0 is instead 'emptySpace').
idempotenceSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
idempotenceSpec = describe "idempotence" $ do
  for_ [("empty", emptySpace @obj), ("full", fullSpace)] $ \(name, obj) ->
    describe name $ do
      prop "idempotent wrt translate" $ \xyz ->
        translate xyz obj
          =~= obj

  prop "empty idempotent wrt scale" $ \xyz ->
    scale xyz emptySpace
      =~= emptySpace @obj

  prop "withRounding always takes the last value idempotent" $ \r r' ->
    withRounding r . withRounding r'
      =~= withRounding @obj r'

------------------------------------------------------------------------------
-- Proofs of the invertability of operations.
inverseSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
inverseSpec = describe "inverses" $ do
  prop "complement inverse" $
    complement @obj . complement
      =~= id

  prop "translate inverse" $ \xyz ->
    translate @obj xyz . translate (negate xyz)
      =~= id

  prop "scale inverse" $
    forAll (arbitrary `suchThat` (not . nearZero)) $ \xyz ->
      scale @obj xyz . scale (1 / xyz)
      =~= id

------------------------------------------------------------------------------
-- Proofs that 'fullSpace' is an annhilative element with respect to union.
annihilationSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
annihilationSpec = describe "annihilation" $ do
  prop "full <> obj = full" $ \obj ->
    fullSpace <> obj
      =~= fullSpace @obj
  prop "obj <> full = full" $ \obj ->
    obj <> fullSpace
      =~= fullSpace @obj

------------------------------------------------------------------------------
-- Misc proofs regarding 2d rotation.
rotation2dSpec :: Spec
rotation2dSpec = describe "2d rotation" $ do
  prop "360 degrees is id" $
    rotate (2 * pi)
      =~= id

  prop "(x + y = 360) degrees is id" $ \rads -> do
      rotate (2 * pi - rads) . rotate rads
        =~= id

  failingProp "rotate" $ \rads1 rads2 ->
    rotate rads2 . rotate rads2
      =~= rotate (rads1 + rads2)

  prop "full idempotent wrt rotate" $ \rads ->
    rotate rads fullSpace
      =~= fullSpace

  prop "empty idempotent wrt rotate" $ \rads ->
    rotate rads emptySpace
      =~= emptySpace

------------------------------------------------------------------------------
-- Misc proofs regarding 3d transformation.
transform2dSpec :: Spec
transform2dSpec = describe "2d transform" $ do
  prop "identity" $
    transform Linear.identity
    =~= id

  prop "same as translation" $ \tr@(V2 x y) ->
    transform
      (V3
        (V3 1 0 x)
        (V3 0 1 y)
        (V3 0 0 1)
      )
    =~= translate tr

------------------------------------------------------------------------------
-- Misc proofs regarding 3d rotation.
rotation3dSpec :: Spec
rotation3dSpec = describe "3d rotation" $ do
  for_ [ ("YZ", V3 1 0 0)
       , ("XZ", V3 0 1 0)
       , ("XY", V3 0 0 1)
       ] $ \(axis, vec) -> do
    describe ("rotation in the " <> axis <> " plane") $ do
      prop "360 degrees is id" $
        rotate3 (vec ^* (2 * pi))
          =~= id
      prop "(x + y = 360) degrees is id" $ \rads ->
        rotate3 (vec ^* (2 * pi - rads)) . rotate3 (vec ^* rads)
          =~= id

  prop "360 degrees is id" $
    forAll (arbitrary `suchThat` (/= pure 0)) $ \vec ->
      rotate3V (2 * pi) vec
        =~= id
  prop "(x + y = 360) degrees is id" $ \rads -> do
    forAll (arbitrary `suchThat` (/= pure 0)) $ \vec ->
      rotate3V (2 * pi - rads) vec . rotate3V rads vec
        =~= id

  prop "rotate" $ \q1 q2 ->
    rotateQ q2 . rotateQ q1
      =~= rotateQ (q2 * q1)

  prop "full idempotent wrt rotate" $ \xyz ->
    rotate3 xyz fullSpace
      =~= fullSpace

  prop "empty idempotent wrt rotate" $ \xyz ->
    rotate3 xyz emptySpace
      =~= emptySpace

------------------------------------------------------------------------------
-- Misc proofs regarding 3d transformation.
transform3dSpec :: Spec
transform3dSpec = describe "3d transform" $ do
  prop "identity" $
    transform3 Linear.identity
    =~= id

  prop "same as rotation and translation" $ \quat tr ->
    transform3 (Linear.mkTransformation quat tr)
    =~= translate tr . rotateQ quat

  prop "scale"
    $ forAll (arbitrary `suchThat` (not . nearZero)) $ \s@(V3 x y z) ->
    transform3
      (V4 (V4 x 0 0 0)
          (V4 0 y 0 0)
          (V4 0 0 z 0)
          (V4 0 0 0 1)
          )
    =~= scale s

  prop "mirror" $
    transform3
    -- mirroring about Y plane
      (V4 (V4 (-1) 0 0 0)
          (V4   0  1 0 0)
          (V4   0  0 1 0)
          (V4   0  0 0 1)
          )
    =~= mirror (V3 1 0 0)

------------------------------------------------------------------------------
-- Misc tests that make sense only in 3d
misc3dSpec :: Spec
misc3dSpec = describe "misc 3d tests" $ do
  prop "object-rounding value doesn't jump from 3d to 2d" $ \r obj ->
    withRounding r . extrude obj
      =~= withRounding r . extrude (withRounding 0 obj)

  prop "cylinder with negative height is a flipped cylinder with positive height" $ \r1 r2 h ->
    cylinder2 r1 r2 h =~= mirror (V3 0 0 1) (cylinder2 r1 r2 (-h))

  prop "negative scale in X is mirror about Y plane" $
    scale @SymbolicObj3 (V3 (-1) 1 1) =~= mirror (V3 1 0 0)

------------------------------------------------------------------------------
-- Misc identity proofs that should hold for all symbolic objects.
identitySpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
identitySpec = describe "identity" $ do
  prop "complement empty" $
    complement @obj emptySpace
      =~= fullSpace

  prop "complement full" $
    complement @obj fullSpace
      =~= emptySpace

  prop "difference of empty" $ \r objs ->
    differenceR @obj r emptySpace objs
      =~= emptySpace

  prop "difference is complement" $ \objs ->
    difference @obj fullSpace objs
      =~= complement (union objs)

  prop "difference of obj" $ \r obj ->
    differenceR @obj r obj []
      =~= obj

  prop "union [a] = a" $ \obj ->
    union @obj [obj] =~= obj

------------------------------------------------------------------------------
-- Functions proving symbolic objects form homomorphisms with respect to
-- translate and scale.
homomorphismSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
homomorphismSpec = describe "homomorphism" $ do
  prop "translate" $ \xyz1 xyz2 ->
    translate @obj xyz2 . translate xyz1
      =~= translate (xyz1 + xyz2)

  prop "scale" $ \xyz1 xyz2 ->
    scale @obj xyz2 . scale xyz1
      =~= scale (xyz1 * xyz2)

  prop "withRounding/unionR" $ \r_obj r_combo ->
    withRounding @obj r_obj . unionR r_combo
      =~= unionR r_combo . fmap (withRounding r_obj)

  prop "withRounding/differenceR" $ \r_obj r_combo obj ->
    withRounding @obj r_obj . differenceR r_combo obj
      =~= differenceR r_combo (withRounding r_obj obj) . fmap (withRounding r_obj)

  prop "withRounding/intersectR" $ \r_obj r_combo ->
    withRounding @obj r_obj . intersectR r_combo
      =~= intersectR r_combo . fmap (withRounding r_obj)

------------------------------------------------------------------------------
-- | Like 'prop', but for tests that are currently expected to fail.
failingProp :: Testable prop => String -> prop -> SpecWith ()
failingProp x = xit (x <> " (currently failing)") . expectFailure . property
