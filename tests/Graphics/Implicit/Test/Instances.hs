{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Test.Instances (Quantizable (quantize), epsilon, observe, (=~=)) where

import Data.VectorSpace (AdditiveGroup((^-^)))
import qualified Graphics.Implicit as I
import Graphics.Implicit
    ( ExtrudeRMScale(Fn, C1, C2),
      SymbolicObj3,
      SymbolicObj2,
      ℝ3,
      ℝ2,
      ℝ,
      squareR,
      Object(shell, translate, unionR, intersectR, differenceR),
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
    ( SymbolicObj3(Cylinder, Complement3, UnionR3, DifferenceR3,
                   IntersectR3, Translate3, Scale3, Rotate3, Rotate3V, Outset3,
                   Shell3, CubeR),
      SymbolicObj2(SquareR, Complement2, UnionR2, DifferenceR2,
                   IntersectR2, Translate2, Scale2, Rotate2, Outset2, Shell2,
                   PolygonR),
      both,
      allthree )
import Graphics.Implicit.Primitives ( Object(getBox, getImplicit) )
import Prelude (Bool (True, False), Int, Double, Integer, (.), flip, uncurry, any, ($), (==), not, (>), (<), and, (&&), all, (>=), length, div, (<*>), (<$>), (+), fmap, (/), fromIntegral, (^), (*), (<>), round, (<=), filter )
import QuickSpec ( Observe(observe), (=~=) )
import Test.QuickCheck
    ( Arbitrary(arbitrary, shrink),
      genericShrink,
      choose,
      oneof,
      scale,
      sized,
      vectorOf,
      Gen,
      Positive(getPositive) )


------------------------------------------------------------------------------
-- | The number of decimal points we need to agree to assume two 'Double's are
-- equal.
epsilon :: Int
epsilon = 5

------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  shrink = filter isValid2 . genericShrink
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
        , polygonR <$> arbitraryPos <*> decayedList
        ]


-- TODO(sandy): Also generate all of the extrusion variants.
instance Arbitrary SymbolicObj3 where
  shrink = filter isValid3 . genericShrink
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
        ]

instance Arbitrary ExtrudeRMScale where
  shrink = genericShrink
  arbitrary = oneof
    [ C1 <$> arbitrary
    , C2 <$> arbitrary
    , Fn <$> arbitrary
    ]


------------------------------------------------------------------------------
-- | Two 'SymbolicObj2's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ2, ()) ℝ SymbolicObj2 where
  observe p = quantize epsilon . observe p . getImplicit


------------------------------------------------------------------------------
-- | Two 'SymbolicObj3's are the same if their 'getImplicit' functions agree at
-- all points (up to an error term of 'epsilon')
instance Observe (ℝ3, ()) ℝ SymbolicObj3 where
  observe p = quantize epsilon . observe p . getImplicit


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

instance Quantizable a => Quantizable (a, a) where
  quantize n = both (quantize n)

instance Quantizable a => Quantizable (a, a, a) where
  quantize n = allthree (quantize n)

instance Quantizable a => Quantizable (b -> a) where
  quantize n = fmap (quantize n)

instance Quantizable Double where
  quantize n r =
    let pow = 10 ^ n :: Double
    in fromIntegral @Integer (round (r * pow)) / pow


------------------------------------------------------------------------------
-- | An implementation of 'Arbitrary' for anything that is an 'Object'.
arbitraryObject :: (Arbitrary a, Arbitrary vec, Object a vec, Quantizable vec) => [Gen a]
arbitraryObject =
  [ translate   <$> fmap (quantize epsilon) arbitrary <*> decayArbitrary 2
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

-- | Generate a quantized, arbitrary positive 'Double'. Useful for sizes.
arbitraryPos :: Gen Double
arbitraryPos = quantize epsilon . getPositive <$> arbitrary

-- | Generate a quantized, arbitrary positive 'ℝ3'. Useful for sizes.
arbitraryV3 :: Gen ℝ3
arbitraryV3 = (,,) <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos

-- | Split the complexity budget by a factor of @n@.
decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


------------------------------------------------------------------------------
-- | Determine if a 'SymbolicObj2' is well-constructed. Ensures we don't
-- accidentally generate a term which will crash when we attempt to render it.
isValid2 :: SymbolicObj2 -> Bool
isValid2 (Complement2 s) = isValid2 s
isValid2 (UnionR2 _ []) = False  -- Bug #304
isValid2 (UnionR2 _ l_s) = all isValid2 l_s
isValid2 (DifferenceR2 _ x l_s) = all isValid2 $ x : l_s
isValid2 (IntersectR2 _ l_s@(_:_:_)) = all isValid2 l_s
isValid2 (IntersectR2 _ _) = False  -- Bug #306
isValid2 (Translate2 _ s) = isValid2 s
isValid2 (Scale2 (x, y) s) = x > 0 && y > 0 && isValid2 s
isValid2 (Rotate2 _ s) = isValid2 s
isValid2 (Outset2 _ s) = isValid2 s
isValid2 (Shell2 _ s) = isValid2 s
isValid2 s@(PolygonR _ ls) = length ls >= 3 &&
  let (dx, dy) = boxSize s
   in not $ any (== 0) [dx, dy]
isValid2 (SquareR _ (x0, y0)) = and
  [ 0 < x0
  , 0 < y0
  ]
isValid2 s =  -- Otherwise, make sure it has > 0 volume
  let (dx, dy) = boxSize s
   in not $ any (== 0) [dx, dy]

-- | Determine if a 'SymbolicObj3' is well-constructed. Ensures we don't
-- accidentally generate a term which will crash when we attempt to render it.
isValid3 :: SymbolicObj3 -> Bool
isValid3 (Complement3 s) = isValid3 s
isValid3 (UnionR3 _ []) = False  -- Bug #304
isValid3 (UnionR3 _ l_s) = all isValid3 l_s
isValid3 (DifferenceR3 _ x l_s) = all isValid3 $ x : l_s
isValid3 (IntersectR3 _ l_s@(_:_:_)) = all isValid3 l_s
isValid3 (IntersectR3 _ _) = False  -- Bug #306
isValid3 (Translate3 _ s) = isValid3 s
isValid3 (Scale3 (x, y, z) s) = x > 0 && y > 0 && z > 0 && isValid3 s
isValid3 (Rotate3 _ s) = isValid3 s
isValid3 (Rotate3V _ _ s) = isValid3 s
isValid3 (Outset3 _ s) = isValid3 s
isValid3 (Shell3 _ s) = isValid3 s
isValid3 (CubeR _ (x0, y0, z0)) = and
  [ 0 < x0
  , 0 < y0
  , 0 < z0
  ]
isValid3 (Cylinder _ r h) = r > 0 && h > 0
isValid3 s =  -- Otherwise, make sure it has > 0 volume
  let (dx, dy, dz) = boxSize s
   in not $ any (== 0) [dx, dy, dz]


------------------------------------------------------------------------------
boxSize :: (Object obj vec, AdditiveGroup vec) => obj -> vec
boxSize = uncurry (flip (^-^)) . getBox

