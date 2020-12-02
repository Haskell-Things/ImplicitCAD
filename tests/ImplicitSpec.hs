{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ImplicitSpec (spec) where

import Prelude ((+), String, Num, Show, Monoid, mempty, (*), (<>), (-), (/=), ($), (.), pi, id)
import Test.Hspec (SpecWith, it, describe, Spec)
import Graphics.Implicit.Test.Instances ((=~=))
import Graphics.Implicit
import Graphics.Implicit.Primitives (rotateQ)
import Test.QuickCheck
    (Testable, property, expectFailure,  Arbitrary(arbitrary),
      suchThat,
      forAll)
import Data.Foldable ( for_ )
import Data.VectorSpace (AdditiveGroup, (^+^),  (^*) )
import Test.Hspec.QuickCheck (prop)
import Graphics.Implicit.Definitions
import QuickSpec (Observe)


------------------------------------------------------------------------------
-- | Tests showing equivalencies between algebraic formulations of symbolic
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

  describe "symbolic obj 3" $ do
    idempotenceSpec  @SymbolicObj3
    identitySpec     @SymbolicObj3
    homomorphismSpec @SymbolicObj3
    monoidSpec       @SymbolicObj3
    inverseSpec      @SymbolicObj3
    annihilationSpec @SymbolicObj3
    rotation3dSpec


------------------------------------------------------------------------------
-- | All the constraints we need in scope to parameterize tests by both 2d and
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
  )


------------------------------------------------------------------------------
-- | Tests proving that symbolic objects form a monoid.
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
-- | Tests showing that 'translate' is a no-op for both 'emptySpace' and
-- 'fullSpace'. Additionally, that 'scale' is a no-op on 'emptySpace' (but not
-- for 'fullSpace', because scaling by 0 is instead 'emptySpace').
idempotenceSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
idempotenceSpec = describe "idempotence" $ do
  for_ [("empty", emptySpace @obj), ("full", fullSpace)] $ \(name, obj) ->
    describe name $ do
      prop "idepotent wrt translate" $ \xyz ->
        translate xyz obj
          =~= obj

  prop "empty idepotent wrt scale" $ \xyz ->
    scale xyz emptySpace
      =~= emptySpace @obj


------------------------------------------------------------------------------
-- | Proofs of the invertability of operations.
inverseSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
inverseSpec = describe "inverses" $ do
  prop "complement inverse" $
    complement @obj . complement
      =~= id


------------------------------------------------------------------------------
-- | Proofs that 'fullSpace' is an annhilative element with respect to union.
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
-- | Misc proofs regarding 2d rotation.
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


------------------------------------------------------------------------------
-- | Misc proofs regarding 3d rotation.
rotation3dSpec :: Spec
rotation3dSpec = describe "3d rotation" $ do
  for_ [ ("YZ", (1, 0, 0))
       , ("XZ", (0, 1, 0))
       , ("XY", (0, 0, 1))
       ] $ \(axis, vec) -> do
    describe ("rotation in the " <> axis <> " plane") $ do
      prop "360 degrees is id" $
        rotate3 (vec ^* (2 * pi))
          =~= id
      prop "(x + y = 360) degrees is id" $ \rads ->
        rotate3 (vec ^* (2 * pi - rads)) . rotate3 (vec ^* rads)
          =~= id

  prop "360 degrees is id" $
    forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
      rotate3V (2 * pi) vec
        =~= id
  prop "(x + y = 360) degrees is id" $ \rads -> do
    forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
      rotate3V (2 * pi - rads) vec . rotate3V rads vec
        =~= id

  prop "rotate" $ \q1 q2 ->
    rotateQ q2 . rotateQ q1
      =~= rotateQ (q2 * q1)


------------------------------------------------------------------------------
-- | Misc identity proofs that should hold for all symbolic objects.
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

  -- TODO(sandy): Broken in 2d due to #328
  prop "difference is complement" $ \objs ->
    difference @obj fullSpace objs
      =~= complement (union objs)

  prop "difference of obj" $ \r obj ->
    differenceR @obj r obj []
      =~= obj

  prop "union [a] = a" $ \obj ->
    union @obj [obj] =~= obj


------------------------------------------------------------------------------
-- | Functions proving symbolic objects form homomorphisms with respect to
-- translate and scale.
homomorphismSpec
    :: forall obj vec test outcome
     . ( TestInfrastructure obj vec test outcome
       , Num vec
       , AdditiveGroup vec
       )
    => Spec
homomorphismSpec = describe "homomorphism" $ do
  prop "translate" $ \xyz1 xyz2 ->
    translate @obj xyz2 . translate xyz1
      =~= translate (xyz1 ^+^ xyz2)

  prop "scale" $ \xyz1 xyz2 ->
    scale @obj xyz2 . scale xyz1
      =~= scale (xyz1 * xyz2)


------------------------------------------------------------------------------
-- | Like 'prop', but for tests that are currently expected to fail.
failingProp :: Testable prop => String -> prop -> SpecWith ()
failingProp x = it x . expectFailure . property

