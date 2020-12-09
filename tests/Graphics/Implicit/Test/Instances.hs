{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Test.Instances (observe, (=~=)) where

import Prelude (abs, fmap, Bounded, Enum, Show, Ord, Eq, (==), pure, Bool (True, False), Int, Double, (.), ($), (<), div, (<*>), (<$>), (+), (<>), (<=))

import Graphics.Implicit
    ( squareR,
      emptySpace,
      fullSpace,
      sphere,
      cubeR,
      cylinder2,
      cylinder,
      circle,
      polygonR,
      extrudeR,
      rotate3,
      rotate3V,
      rotate )

import Graphics.Implicit.Definitions
    ( ExtrudeRMScale(C1,C2,Fn),
      SymbolicObj2(Shared2),
      SymbolicObj3(Shared3),
      ℝ,
      ℝ2,
      ℝ3,
      SharedObj(Outset, Translate, Scale, UnionR, IntersectR,
                DifferenceR, Shell) )

import Graphics.Implicit.Primitives ( Object(getImplicit) )

import QuickSpec ( Observe(observe), (=~=) )

import Test.QuickCheck
    (CoArbitrary(coarbitrary), discard,  Arbitrary(arbitrary, shrink),
      genericShrink,
      choose,
      oneof,
      scale,
      sized,
      vectorOf,
      Gen,
      Positive(getPositive) )

import Linear (quadrance, V2(V2), V3(V3), Quaternion, axisAngle)

data Insidedness = Inside | Outside | Surface
  deriving (Eq, Ord, Show, Enum, Bounded)

insidedness :: Double -> Insidedness
insidedness 0 = Surface
insidedness x =
  case x < 0 of
    True  -> Inside
    False -> Outside

------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate <$> arbitrary <*> decayArbitrary 2
        , Shared2 <$> arbitrary
        ] <> small
      True -> oneof small
    where
      small =
        [ circle   <$> arbitrary
        , squareR  <$> arbitraryPos <*> arbitrary <*> arbitrary
        , polygonR <$> arbitraryPos <*> do
            n <- choose (3, 10)
            vectorOf n arbitrary
        , pure fullSpace
        , pure emptySpace
        ]


-- TODO(sandy): Also generate all of the extrusion variants.
instance Arbitrary SymbolicObj3 where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate3  <$> arbitrary    <*> decayArbitrary 2
        , rotate3V <$> arbitrary    <*> arbitrary        <*> decayArbitrary 2
        , extrudeR <$> arbitraryPos <*> decayArbitrary 2 <*> arbitraryPos
        , Shared3 <$> arbitrary
        ] <> small
      True -> oneof small
    where
      small =
        [ sphere    <$> arbitraryPos
        , cylinder  <$> arbitraryPos <*> arbitraryPos
        , cylinder2 <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
        , cubeR     <$> arbitraryPos <*> arbitrary    <*> arbitraryV3
        , pure fullSpace
        , pure emptySpace
        ]

instance (Arbitrary obj, Arbitrary vec, CoArbitrary vec) => Arbitrary (SharedObj obj vec) where
  shrink = genericShrink
  arbitrary = oneof
    [ Translate   <$> arbitrary <*> decayArbitrary 2
    , Scale       <$> arbitrary <*> decayArbitrary 2
    , UnionR      <$> arbitraryPos <*> decayedList
    , IntersectR  <$> arbitraryPos <*> decayedList
    , DifferenceR <$> arbitraryPos <*> decayArbitrary 2 <*> decayedList
    , Shell       <$> arbitraryPos <*> decayArbitrary 2
    , Outset      <$> arbitraryPos <*> decayArbitrary 2
    ]

instance Arbitrary ℝ2 where
  shrink = genericShrink
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary ℝ3 where
  shrink = genericShrink
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary ℝ2 where
  coarbitrary (V2 a b) = coarbitrary (a, b)

instance CoArbitrary ℝ3 where
  coarbitrary (V3 a b c) = coarbitrary (a, b, c)


instance Arbitrary ExtrudeRMScale where
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
    case quadrance v == 0.0 of
      True  -> discard
      False -> pure $ axisAngle v q


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

