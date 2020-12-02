{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ImplicitSpec (spec) where

import Prelude (String, Num, Show, Monoid, mempty, (*), (<>), (-), (/=), ($), (.), pi, id)
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


spec :: Spec
spec = do
  describe "symbolic obj 2" $ do
    idempotenceSpec @SymbolicObj2
    identitySpec @SymbolicObj2
    homomorphismSpec @SymbolicObj2
    monoidSpec @SymbolicObj2
    inverseSpec @SymbolicObj2
    annihilationSpec @SymbolicObj2
  describe "symbolic obj 3" $ do
    idempotenceSpec @SymbolicObj3
    identitySpec @SymbolicObj3
    homomorphismSpec @SymbolicObj3
    monoidSpec @SymbolicObj3
    inverseSpec @SymbolicObj3
    annihilationSpec @SymbolicObj3


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

monoidSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
monoidSpec = describe "monoid" $ do
  describe "monoid laws" $ do
    prop "a <> mempty = a" $ \obj ->
      obj =~= obj <> mempty @obj
    prop "mempty <> a = a" $ \obj ->
      obj =~= mempty @obj <> obj
    prop "(a <> b) <> c = a <> (b <> c)" $ \a b (c :: obj) ->
      (a <> b) <> c =~= a <> (b <> c)

  prop "union [a] = a" $ \obj ->
    union @obj [obj] =~= obj


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
      -- prop "idepotent wrt rotate" $ \xyz ->
      --   rotate3 xyz obj
      --     =~= obj

  prop "empty idepotent wrt scale" $ \xyz ->
    scale xyz emptySpace
      =~= emptySpace @obj


inverseSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
inverseSpec = describe "inverses" $ do
  prop "complement inverse" $
    complement @obj . complement
      =~= id

annihilationSpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
annihilationSpec = describe "annihilation" $ do
  prop "union/full" $ \obj ->
    fullSpace <> obj
      =~= fullSpace @obj
  prop "union/full" $ \obj ->
    obj <> fullSpace
      =~= fullSpace @obj


identitySpec
    :: forall obj vec test outcome
     . TestInfrastructure obj vec test outcome
    => Spec
identitySpec = describe "identity" $ do
  describe "getImplicit" $ do
    for_ [ ("YZ", (1, 0, 0))
         , ("XZ", (0, 1, 0))
         , ("XY", (0, 0, 1))
         ] $ \(axis, vec) -> do
      describe ("rotation in the " <> axis <> " plane (observed by getImplicit)") $ do
        prop "360 degrees is id" $
          rotate3 (vec ^* (2 * pi))
            =~= id
        prop "(x + y = 360) degrees is id" $ \rads ->
          rotate3 (vec ^* (2 * pi - rads)) . rotate3 (vec ^* rads)
            =~= id

  describe "rotation in arbitrary planes (observed by getImplicit)" $ do
    prop "360 degrees is id" $
      forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
        rotate3V (2 * pi) vec
          =~= id
    prop "(x + y = 360) degrees is id" $ \rads -> do
      forAll (arbitrary `suchThat` (/= (0, 0, 0))) $ \vec ->
        rotate3V (2 * pi - rads) vec . rotate3V rads vec
          =~= id

  prop "complement inverse" $
    complement @obj . complement
      =~= id

  prop "complement empty" $
    complement @obj emptySpace
      =~= fullSpace

  prop "complement full" $
    complement @obj fullSpace
      =~= emptySpace

  prop "difference of empty" $ \r objs ->
    differenceR @obj r emptySpace objs
      =~= emptySpace

  failingProp "difference is complement" $ \objs ->
    difference @obj fullSpace objs
      =~= complement (union objs)

  prop "difference of obj" $ \r obj ->
    differenceR @obj r obj []
      =~= obj


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

  prop "rotate" $ \q1 q2 ->
    rotateQ q2 . rotateQ q1
      =~= rotateQ (q2 * q1)


failingProp :: Testable prop => String -> prop -> SpecWith ()
failingProp x = it x . expectFailure . property

