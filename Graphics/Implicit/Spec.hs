{-# LANGUAGE FlexibleInstances                     #-}
{-# LANGUAGE LambdaCase                            #-}
{-# LANGUAGE MultiParamTypeClasses                 #-}
{-# LANGUAGE NumDecimals                           #-}
{-# LANGUAGE TypeApplications                      #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists     #-}
{-# OPTIONS_GHC -fno-warn-monomorphism-restriction #-}
{-# OPTIONS_GHC -fno-warn-orphans                  #-}

module Graphics.Implicit.Spec where

import           Data.Bool (bool)
import           Data.List (sort)
import           Data.VectorSpace (AdditiveGroup((^-^)))
import           Debug.Trace (traceShowId)
import qualified Graphics.Implicit as I
import           Graphics.Implicit hiding (scale)
import           Graphics.Implicit.Definitions
import           Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable(discreteAprox))
import           Graphics.Implicit.Primitives hiding (scale)
import           Prelude
import           QuickSpec
import           Test.QuickCheck hiding (Fn)


------------------------------------------------------------------------------
instance Arbitrary SymbolicObj2 where
  shrink = filter isValid2 . genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate <$> arbitrary <*> decayArbitrary 2
        ] ++ arbitraryObject ++ small
      True -> oneof small
    where
      small =
        [ circle   <$> arbitrary
        , rectR    <$> arbitraryPos <*> arbitrary <*> arbitrary
        , polygonR <$> arbitraryPos <*> decayedList
        ]


instance Arbitrary SymbolicObj3 where
  shrink = filter isValid3 . genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate3  <$> arbitrary    <*> decayArbitrary 2
        , rotate3V <$> arbitrary    <*> arbitrary        <*> decayArbitrary 2
        , extrudeR <$> arbitraryPos <*> decayArbitrary 2 <*> arbitraryPos
        ] ++ arbitraryObject ++ small
      True -> oneof small
    where
      small =
        [ -- sphere    <$> arbitraryPos
        -- , cylinder  <$> arbitraryPos <*> arbitraryPos
        -- , cylinder2 <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
          cubeR    <$> arbitraryPos <*> arbitrary <*> arbitraryV3
        ]

instance Arbitrary ExtrudeRMScale where
  shrink = genericShrink
  arbitrary = oneof
    [ C1 <$> arbitrary
    , C2 <$> arbitrary
    , Fn <$> arbitrary
    ]


------------------------------------------------------------------------------
instance Observe () [Polyline] SymbolicObj2 where
  observe _ = quantize 2 . discreteAprox 1


instance Observe () TriangleMesh SymbolicObj3 where
  observe _ obj
    = quantize 2
    . discreteAprox sample_size
    $ obj
    where
      (origin, extent) = getBox obj
      (dx, dy, dz) = extent ^-^ origin
      sample_size = abs $ (dx * dy * dz) / 10


------------------------------------------------------------------------------
class Quantizable a where
  quantize :: Int -> a -> a

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
    let pow = 10 ^ n
    in fromIntegral @Integer (round (r * pow)) / pow

instance Quantizable Polyline where
  quantize n = Polyline . quantize n . getPolylinePoints

instance Quantizable Triangle where
  quantize n = Triangle . quantize n . getTrianglePoints

instance Quantizable TriangleMesh where
  quantize n = TriangleMesh . quantize n . getTriangleMeshTriangles



------------------------------------------------------------------------------
arbitraryObject :: (Arbitrary a, Arbitrary vec, Object a vec, Quantizable vec) => [Gen a]
arbitraryObject =
  [ translate   <$> fmap (quantize 3) arbitrary <*> decayArbitrary 2
  , I.scale     <$> arbitrary <*> decayArbitrary 2
  , unionR      <$> arbitraryPos <*> decayedList
  , intersectR  <$> arbitraryPos <*> decayedList
  , differenceR <$> arbitraryPos <*> decayArbitrary 2 <*> decayedList
  , shell       <$> arbitraryPos <*> decayArbitrary 2
  ]


decayedList :: Arbitrary a => Gen [a]
decayedList = do
  n <- choose (1, 10)
  vectorOf n $ decayArbitrary $ n + 1

arbitraryPos :: Gen Double
arbitraryPos = quantize 3 . getPositive <$> arbitrary

arbitraryV3 :: Gen ℝ3
arbitraryV3 = (,,) <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos


decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


------------------------------------------------------------------------------
isValid2 :: SymbolicObj2 -> Bool
isValid2 (Complement2 s) = isValid2 s
isValid2 (UnionR2 _ []) = False
isValid2 (UnionR2 _ l_s) = all isValid2 l_s
isValid2 (DifferenceR2 _ x l_s) = all isValid2 $ x : l_s
isValid2 (IntersectR2 _ l_s@(_:_:_)) = all isValid2 l_s
isValid2 (IntersectR2 _ _) = False
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
isValid2 s =
  let (dx, dy) = boxSize s
   in not $ any (== 0) [dx, dy]

isValid3 :: SymbolicObj3 -> Bool
isValid3 (Complement3 s) = isValid3 s
isValid3 (UnionR3 _ []) = False
isValid3 (UnionR3 _ l_s) = all isValid3 l_s
isValid3 (DifferenceR3 _ x l_s) = all isValid3 $ x : l_s
isValid3 (IntersectR3 _ l_s@(_:_:_)) = all isValid3 l_s
isValid3 (IntersectR3 _ _) = False
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
isValid3 s =
  let (dx, dy, dz) = boxSize s
   in not $ any (== 0) [dx, dy, dz]


------------------------------------------------------------------------------
boxSize :: (Object obj vec, AdditiveGroup vec) => obj -> vec
boxSize = uncurry (flip (^-^)) . getBox


sig :: Sig
sig = signature
  [ con "nothing" $ rect3R 0 (0, 0, 0) (0, 0, 0)
  , con "sphere" sphere
  , con "rect3R" rect3R
  , con "rotate3" rotate3
  , con "rectR" rectR
  , con "extrudeR" extrudeR
  , con "," $ (,) @Double @Double
  , con ",," $ (,,) @Double @Double @Double
  , monoObserve @SymbolicObj3
  , monoObserve @SymbolicObj2
  , mono @Double
  , mono @(Double, Double)
  , mono @(Double, Double, Double)
  , withMaxTermSize 12
  ]


------------------------------------------------------------------------------
test :: IO ()
test = quickCheck $ \r (Small x1) (Small x2) (Small y1) (Small y2) (Small z1) (Small z2) ->
  let [x1', x2'] = fmap fromInteger $ sort [x1, x2]
      [y1', y2'] = fmap fromInteger $ sort [y1, y2]
      [z1', z2'] = fmap fromInteger $ sort [z1, z2]
      origin = (x1', y1', z1')
   in rect3R r origin (x2', y2', z2')
  =~= translate origin (rect3R r (0, 0, 0) (x2' - x1', y2' - y1', y2' - z1'))

test2 :: IO ()
test2 = quickCheckWith (stdArgs {maxSuccess = 10000}) $ \obj ->
      (quantize 2) (getBox obj)
  =~= (quantize 2) (getBox (rotate3 (2 * pi, 0, 0) obj))

