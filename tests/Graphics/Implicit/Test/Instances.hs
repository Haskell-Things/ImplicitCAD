{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julia.longtin@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE GADTs                  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Test.Instances (Observe, observe, (=~=)) where

import Prelude (abs, fmap, Bounded, Double, Enum, Show, Ord, Eq, (==), pure, Int, Double, (.), ($), (<), div, (<*>), (<$>), (+), (<>), (<=))

import Graphics.Implicit
    ( square,
      emptySpace,
      fullSpace,
      sphere,
      cube,
      cylinder2,
      cylinder,
      circle,
      polygon,
      extrude,
      rotate3,
      rotate3V,
      transform3,
      rotate )

import Graphics.Implicit.Definitions
    ( ExtrudeMScale(C1,C2,Fn),
      SymbolicObj2(Shared2),
      SymbolicObj3(Shared3),
      ℝ,
      ℝ2,
      ℝ3,
      SharedObj(Outset, Translate, Scale, UnionR, IntersectR,
                DifferenceR, Shell, WithRounding) )

import Graphics.Implicit.Primitives ( getImplicit )

import qualified Test.QuickCheck
import Test.QuickCheck
    (CoArbitrary(coarbitrary), discard,  Arbitrary(arbitrary, shrink),
      genericShrink,
      choose,
      oneof,
      scale,
      sized,
      vectorOf,
      Gen,
      Positive(getPositive),
      Property)

import Linear (V2(V2), V3(V3), V4(V4), Quaternion, axisAngle)

data Insidedness = Inside | Outside | Surface
  deriving (Eq, Ord, Show, Enum, Bounded)

insidedness :: Double -> Insidedness
insidedness 0 = Surface
insidedness x = if x < 0 then Inside else Outside

------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    if n <= 1
    then oneof small
    else oneof $
        [ rotate <$> arbitrary <*> decayArbitrary 2
        , Shared2 <$> arbitrary
        ] <> small
    where
      small =
        [ circle  <$> arbitrary
        , square  <$> arbitrary <*> arbitrary
        , polygon <$> do
            n <- choose (3, 10)
            vectorOf n arbitrary
        , pure fullSpace
        , pure emptySpace
        ]

-- TODO(sandy): Also generate all of the extrusion variants.
instance Arbitrary SymbolicObj3 where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    if n <= 1
    then oneof small
    else oneof $
        [ rotate3    <$> arbitrary        <*> decayArbitrary 2
        , rotate3V   <$> arbitrary        <*> arbitrary        <*> decayArbitrary 2
        , transform3 <$> arbitrary        <*> decayArbitrary 2
        , extrude    <$> decayArbitrary 2 <*> arbitraryPos
        , Shared3    <$> arbitrary
        ] <> small
    where
      small =
        [ sphere    <$> arbitraryPos
        , cylinder  <$> arbitraryPos <*> arbitraryPos
        , cylinder2 <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
        , cube      <$> arbitrary    <*> arbitraryV3
        , pure fullSpace
        , pure emptySpace
        ]

instance (Arbitrary obj, Arbitrary vec, CoArbitrary vec) => Arbitrary (SharedObj obj vec) where
  shrink = genericShrink
  arbitrary = oneof
    [ Translate    <$> arbitrary    <*> decayArbitrary 2
    , Scale        <$> arbitrary    <*> decayArbitrary 2
    , UnionR       <$> arbitraryPos <*> decayedList
    , IntersectR   <$> arbitraryPos <*> decayedList
    , DifferenceR  <$> arbitraryPos <*> decayArbitrary 2 <*> decayedList
    , Shell        <$> arbitraryPos <*> decayArbitrary 2
    , Outset       <$> arbitraryPos <*> decayArbitrary 2
    , WithRounding <$> arbitraryPos <*> decayArbitrary 2
    ]

instance Arbitrary a => Arbitrary (V2 a) where
  shrink = genericShrink
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
  shrink = genericShrink
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V4 a) where
  shrink = genericShrink
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary a => CoArbitrary (V2 a) where
  coarbitrary (V2 a b) = coarbitrary (a, b)

instance CoArbitrary a => CoArbitrary (V3 a) where
  coarbitrary (V3 a b c) = coarbitrary (a, b, c)

instance CoArbitrary a => CoArbitrary (V4 a) where
  coarbitrary (V4 a b c d) = coarbitrary (a, b, c, d)

instance Arbitrary ExtrudeMScale where
  shrink = genericShrink
  arbitrary = oneof
    [ C1 <$> arbitrary
    , C2 <$> arbitrary
    , Fn <$> arbitrary
    ]

instance Arbitrary (Quaternion ℝ) where
  arbitrary = do
    q <- arbitrary
    v <- arbitraryV3
    if v == 0.0
      then discard
      else pure $ axisAngle v q

------------------------------------------------------------------------------
-- Minimum of quickspec(s) Observe class and instances required for implicit testsuite
-- BSD3 Copyright: 2009-2019 Nick Smallbone
class (Arbitrary test, Ord outcome) => Observe test outcome a | a -> test outcome where
  -- | Make an observation on a value. Should satisfy the following law: if
  -- @x /= y@, then there exists a value of @test@ such that @observe test x /= observe test y@.
  observe :: test -> a -> outcome

  default observe :: (outcome ~ a) => test -> a -> outcome
  observe _ x = x

instance (Arbitrary a, Observe test outcome b) => Observe (a, test) outcome (a -> b) where
  observe (x, obs) f = observe obs (f x)

instance Observe () Double Double

(=~=) :: (Show test, Show outcome, Observe test outcome a) => a -> a -> Property
a =~= b = Test.QuickCheck.property $ \test -> observe test a Test.QuickCheck.=== observe test b
infix 4 =~=

------------------------------------------------------------------------------
-- | Two 'SymbolicObj2's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ2, ()) Insidedness SymbolicObj2 where
  observe p = insidedness . observe p . getImplicit

------------------------------------------------------------------------------
-- | Two 'SymbolicObj3's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ3, ()) Insidedness SymbolicObj3 where
  observe p = insidedness . observe p . getImplicit

-- | Generate a small list of 'Arbitrary' elements, splitting the current
-- complexity budget between all of them.
decayedList :: Arbitrary a => Gen [a]
decayedList = do
  n <- choose (1, 10)
  vectorOf n $ decayArbitrary $ n + 1

-- | Generate an arbitrary positive 'Double'. Useful for sizes.
arbitraryPos :: Gen Double
arbitraryPos = getPositive <$> arbitrary

-- | Generate an arbitrary positive 'ℝ3'. Useful for sizes.
arbitraryV3 :: Gen ℝ3
arbitraryV3 = fmap abs <$> arbitrary

-- | Split the complexity budget by a factor of @n@.
decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

