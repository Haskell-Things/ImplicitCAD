{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Test.Instances (epsilon, observe, (=~=)) where

import Prelude (Bounded, Enum, Show, Ord, Eq, (==), pure, Bool (True, False), Int, Double, (.), flip, uncurry, ($), (>), (<), (&&), all, (>=), length, div, (<*>), (<$>), (+), (<>), (<=), filter, notElem)

import Data.VectorSpace (magnitudeSq, AdditiveGroup((^-^)))

import qualified Graphics.Implicit as I (scale)

import Graphics.Implicit
    ( ExtrudeRMScale(Fn, C1, C2),
      SymbolicObj3,
      SymbolicObj2,
      ℝ3,
      ℝ2,
      ℝ,
      squareR,
      Object(shell, translate, unionR, intersectR, differenceR, emptySpace, fullSpace),
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

import Graphics.Implicit.Primitives ( Object(getBox, getImplicit) )

import QuickSpec ( Observe(observe), (=~=) )

import Test.QuickCheck
    (discard,  Arbitrary(arbitrary, shrink),
      genericShrink,
      choose,
      oneof,
      scale,
      sized,
      vectorOf,
      Gen,
      Positive(getPositive) )

import Data.List (nub)
import Linear (Quaternion, axisAngle)
import Graphics.Implicit.MathUtil (packV3)
import Data.Bool (bool)


data Insidedness = Inside | Outside | Surface
  deriving (Eq, Ord, Show, Enum, Bounded)


insidedness :: Double -> Insidedness
insidedness 0 = Surface
insidedness x =
  case x < 0 of
    True  -> Inside
    False -> Outside

------------------------------------------------------------------------------
-- | The number of decimal points we need to agree to assume two 'Double's are
-- equal.
epsilon :: Int
epsilon = 5

------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  -- shrink = filter isValid2 . genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate <$> arbitrary <*> decayArbitrary 2
        ] <> arbitraryObject <> small
      True -> oneof small
    where
      small =
        [ circle   <$> arbitrary
        , squareR  <$> arbitraryPos <*> arbitrary <*> arbitrary
        , polygonR <$> arbitraryPos <*> do
            n <- choose (5, 10)
            v <- nub <$> vectorOf n arbitrary
            pure $ bool discard v $ length v >= 3
        , pure fullSpace
        , pure emptySpace
        ]


-- TODO(sandy): Also generate all of the extrusion variants.
instance Arbitrary SymbolicObj3 where
  -- shrink = filter isValid3 . genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate3  <$> arbitrary    <*> decayArbitrary 2
        , rotate3V <$> arbitrary    <*> arbitrary        <*> decayArbitrary 2
        , extrudeR <$> arbitraryPos <*> decayArbitrary 2 <*> arbitraryPos
        ] <> arbitraryObject <> small
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
    case magnitudeSq v == 0.0 of
      True  -> discard
      False -> pure $ axisAngle (packV3 v) q


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



------------------------------------------------------------------------------
-- | An implementation of 'Arbitrary' for anything that is an 'Object'.
arbitraryObject :: (Arbitrary a, Arbitrary vec, Object a vec) => [Gen a]
arbitraryObject =
  [ translate   <$> arbitrary <*> decayArbitrary 2
  , I.scale     <$> arbitrary <*> decayArbitrary 2
  , unionR      <$> arbitraryPos <*> decayedList
  , intersectR  <$> arbitraryPos <*> decayedList
  , differenceR <$> arbitraryPos <*> decayArbitrary 2 <*> decayedList
  , shell       <$> arbitraryPos <*> decayArbitrary 2
  ]


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
arbitraryV3 = (,,) <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos

-- | Split the complexity budget by a factor of @n@.
decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


------------------------------------------------------------------------------
-- | Determine if a 'SymbolicObj2' is well-constructed. Ensures we don't
-- accidentally generate a term which will crash when we attempt to render it.
isValid2 :: SymbolicObj2 -> Bool
isValid2 (Shared2 (Complement s)) = isValid2 s
isValid2 (Shared2 (UnionR _ [])) = False  -- Bug #304
isValid2 (Shared2 (UnionR _ l_s)) = all isValid2 l_s
isValid2 (Shared2 (DifferenceR _ x l_s)) = all isValid2 $ x : l_s
isValid2 (Shared2 (IntersectR _ l_s@(_:_:_))) = all isValid2 l_s
isValid2 (Shared2 (IntersectR _ _)) = False  -- Bug #306
isValid2 (Shared2 (Translate _ s)) = isValid2 s
isValid2 (Shared2 (Scale (x, y) s)) = x > 0 && y > 0 && isValid2 s
isValid2 (Rotate2 _ s) = isValid2 s
isValid2 (Shared2 (Outset _ s)) = isValid2 s
isValid2 (Shared2 (Shell _ s)) = isValid2 s
isValid2 s@(PolygonR _ ls) = length ls >= 3 && nub ls == ls

-- | Determine if a 'SymbolicObj3' is well-constructed. Ensures we don't
-- accidentally generate a term which will crash when we attempt to render it.
isValid3 :: SymbolicObj3 -> Bool
isValid3 (Shared3 (Complement s)) = isValid3 s
isValid3 (Shared3 (UnionR _ [])) = False  -- Bug #304
isValid3 (Shared3 (UnionR _ l_s)) = all isValid3 l_s
isValid3 (Shared3 (DifferenceR _ x l_s)) = all isValid3 $ x : l_s
isValid3 (Shared3 (IntersectR _ l_s@(_:_:_))) = all isValid3 l_s
isValid3 (Shared3 (IntersectR _ _)) = False  -- Bug #306
isValid3 (Shared3 (Translate _ s)) = isValid3 s
isValid3 (Shared3 (Scale (x, y, z) s)) = x > 0 && y > 0 && z > 0 && isValid3 s
isValid3 (Rotate3 _ s) = isValid3 s
isValid3 (Shared3 (Outset _ s)) = isValid3 s
isValid3 (Shared3 (Shell _ s)) = isValid3 s
isValid3 (CubeR _ (x0, y0, z0)) = (0 < x0) && (0 < y0) && (0 < z0)
isValid3 (Cylinder _ r h) = r > 0 && h > 0
isValid3 s =  -- Otherwise, make sure it has > 0 volume
  let (dx, dy, dz) = boxSize s
   in all (> 0) [dx, dy, dz]


------------------------------------------------------------------------------
boxSize :: (Object obj vec, AdditiveGroup vec) => obj -> vec
boxSize = uncurry (flip (^-^)) . getBox

