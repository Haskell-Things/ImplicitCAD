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
{-# LANGUAGE TypeOperators          #-}
-- type (~)
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- Polymorphic makeTestResult
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Test.Instances (Observe, observe, (=~=), arbitraryNonZeroV) where

import Prelude (Applicative, (.), not, abs, fmap, Bool(False, True), Bounded, Double, Integer, fromIntegral, (*), (/), (^), round, Enum, Show(show), unlines, Ord, compare, Eq, (==), pure, RealFloat(isNaN), Int, Double, ($), (<), div, (<*>), (<$>), (+), (<>), (<=))
#if MIN_VERSION_base(4,17,0)
import Prelude (type(~))
#endif

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
      rotate,
      transform )

import Graphics.Implicit.Definitions
    ( ExtrudeMScale(C1,C2,Fn),
      SymbolicObj2(Shared2),
      SymbolicObj3(Shared3),
      ℝ,
      ℝ2,
      ℝ3,
      SharedObj(Outset, Translate, Scale, UnionR, IntersectR,
                DifferenceR, Shell, WithRounding) )

import Graphics.Implicit.Primitives ( getImplicit, Object(Space) )

import qualified Test.QuickCheck
import Test.QuickCheck
    (CoArbitrary(coarbitrary), discard,  Arbitrary(arbitrary, shrink),
      genericShrink,
      choose,
      oneof,
      scale,
      sized,
      suchThat,
      vectorOf,
      Gen,
      Positive(getPositive),
      NonZero(getNonZero),
      Property)

import Linear (V2(V2), V3(V3), V4(V4), M33, det33, M44, det44, Epsilon, nearZero, Quaternion, axisAngle)

data Insidedness = Inside | Outside | Surface | NaNFail
  deriving (Ord, Show, Enum, Bounded)

insidedness :: (RealFloat a) => a -> Insidedness
insidedness 0 = Surface
insidedness x | isNaN x = NaNFail
insidedness x | x < 0 = Inside
insidedness _ = Outside

-- Explicitely allow matching the three cases
-- so NaNFail never passes Eq (similar to an actual NaNs
-- that are never equal)
instance Eq Insidedness where
  Inside == Inside = True
  Outside == Outside = True
  Surface == Surface = True
  _ == _ = False

data TestResult obj a = TestResult {
    trInsidedness   :: Insidedness
  , trSampledValue  :: a
  , trSampledObject :: obj
  , trSampledAt     :: (Space obj) a
  }

instance
  ( Show obj
  , Show a
  , Show (Space obj a)
  )
  => Show (TestResult obj a)
  where
  show TestResult{..} = unlines
    [ ""
    , "TestResult:"
    , "  | " <> show trInsidedness
    , "  | " <> show trSampledObject
    , "  | Sampled at " <> show trSampledAt <> " returns " <> show trSampledValue
    ]

instance Eq a => Eq (TestResult obj a) where
  (==) ta tb = trInsidedness ta == trInsidedness tb

instance Ord a => Ord (TestResult obj a) where
  compare ta tb = trInsidedness ta `compare` trInsidedness tb

------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    if n <= 1
    then oneof small
    else oneof $
        [ rotate <$> arbitrary <*> decayArbitrary 2
        , transform <$> arbitraryInvertibleM33 <*> decayArbitrary 2
        , Shared2 <$> arbitrary
        ] <> small
    where
      small =
        [ circle  <$> arbitrary
        , square  <$> arbitrary <*> arbitrary
        , polygon <$> do
            n <- choose (3, 10)
            -- TODO(srk): this is a hack until #449 is solved
            vectorOf n ((*100) <$> (V2 <$> arbitraryPos <*> arbitraryPos))
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
        , transform3 <$> arbitraryInvertibleM44 <*> decayArbitrary 2
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

instance (Arbitrary obj, Arbitrary a, Arbitrary (f a), CoArbitrary (f a)) => Arbitrary (SharedObj obj f a) where
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

makeTestResult
  :: forall obj f a
   . ( RealFloat a
     , Object obj f a
     , Quantizable a
     , f ~ Space obj
     )
  => obj
  -> Space obj a
  -> TestResult obj a
makeTestResult obj sampleAt =
  let
    fun = getImplicit obj
    sampledVal = quantize epsilon $ fun sampleAt
  in
    TestResult
      { trInsidedness = insidedness sampledVal
      , trSampledValue = sampledVal
      , trSampledObject = obj
      , trSampledAt = sampleAt
      }

------------------------------------------------------------------------------
-- | Two 'SymbolicObj2's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ2, ()) (TestResult SymbolicObj2 Double) SymbolicObj2 where
  observe (sampledAt, _) obj = makeTestResult obj sampledAt

------------------------------------------------------------------------------
-- | Two 'SymbolicObj3's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ3, ()) (TestResult SymbolicObj3 Double) SymbolicObj3 where
  observe (sampledAt, _) obj = makeTestResult obj sampledAt

------------------------------------------------------------------------------
-- | The number of decimal points we need to agree to assume two 'Double's are
-- equal.
epsilon :: Int
epsilon = 10

------------------------------------------------------------------------------
-- | Types which can truncate their decimal points to a certain number of
-- digits.
class Quantizable a where
  quantize
      :: Int  -- ^ The number of decimal points to keep
      -> a
      -> a

instance Quantizable a => Quantizable [a] where
  quantize n = fmap (quantize n)

instance Quantizable a => Quantizable (b -> a) where
  quantize n = fmap (quantize n)

instance Quantizable Double where
  quantize n r =
    let pow = 10 ^ n :: Double
    in fromIntegral @Integer (round (r * pow)) / pow

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

-- | Generate arbitrary vector that has no zero components
arbitraryNonZeroV
  :: ( Arbitrary (f (NonZero a))
     , Applicative f
     )
  => Gen (f a)
arbitraryNonZeroV = fmap getNonZero <$> arbitrary

-- | Generate arbitrary invertible 3x3 matrix, representing
-- affine transformation matrix in 2D space. The last vector is fixed
-- to @V3 0 0 1@ so it doesn't result in NaNs when normalized from
-- homogeneous coordinates.
--
-- Inspired by InvertibleM33 from linear-tests package
-- https://github.com/minimapletinytools/linear-tests/blob/master/src/Linear/Matrix/Arbitrary.hs
arbitraryInvertibleM33
  :: ( Arbitrary a
     , Epsilon a
     )
  => Gen (M33 a)
arbitraryInvertibleM33 =
  (V3 <$> arbitrary <*> arbitrary <*> pure (V3 0 0 1))
    `suchThat` (not . nearZero . det33)

-- | Generate arbitrary invertible 4x4 matrix, representing
-- affine transformation matrix in 3D space. The last vector is fixed
-- to @V4 0 0 0 1@ so it doesn't result in NaNs when normalized from
-- homogeneous coordinates.
arbitraryInvertibleM44
  :: ( Arbitrary a
     , Epsilon a
     )
  => Gen (M44 a)
arbitraryInvertibleM44 =
  (V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> pure (V4 0 0 0 1))
    `suchThat` (not . nearZero . det44)

-- | Split the complexity budget by a factor of @n@.
decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

