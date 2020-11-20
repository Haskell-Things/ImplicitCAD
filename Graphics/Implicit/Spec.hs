{-# LANGUAGE FlexibleInstances                     #-}
{-# LANGUAGE LambdaCase                            #-}
{-# LANGUAGE MultiParamTypeClasses                 #-}
{-# LANGUAGE NumDecimals                           #-}
{-# LANGUAGE TypeApplications                      #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists     #-}
{-# OPTIONS_GHC -fno-warn-monomorphism-restriction #-}
{-# OPTIONS_GHC -fno-warn-orphans                  #-}

module Graphics.Implicit.Spec where

import           Data.List (sort)
import qualified Graphics.Implicit as I
import           Graphics.Implicit hiding (scale)
import           Graphics.Implicit.Definitions
import           Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable(discreteAprox))
import           Graphics.Implicit.Primitives hiding (scale)
import           Prelude
import           QuickSpec
import           Test.QuickCheck hiding (Fn)
import Data.VectorSpace (AdditiveGroup((^-^)))
import Debug.Trace (traceShowId)
import Data.Bool (bool)


instance Arbitrary SymbolicObj2 where
  shrink = filter isValid2 . genericShrink
  arbitrary = fmap (\x -> bool discard x $ isValid2 x) $ sized $ \n ->
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


instance Observe () TriangleMesh SymbolicObj3 where
  observe _ obj
    = TriangleMesh
    . fmap ( Triangle   -- this fmap trims floats to 2 decimal places
           . allthree (allthree $ quantize 2)
           . getTrianglePoints
           )
    . getTriangleMeshTriangles
    . discreteAprox sample_size
    $ obj
    where
      (origin, extent) = getBox obj
      (dx, dy, dz) = extent ^-^ origin
      sample_size = abs $ (dx * dy * dz) / 10


        -- implicit @SymbolicObj3
        --   (getImplicit obj)
          -- ((0, 0, 0), (1, 1, 1))

instance Observe () [Polyline] SymbolicObj2 where
  observe _ = fmap (Polyline . fmap (both $ quantize 2) . getPolylinePoints) . discreteAprox 1

quantize
  :: (RealFrac a) => Int -> a -> a
quantize n r =
  let pow = 10 ^ n
   in fromIntegral @Integer (round (r * pow)) / pow


instance Arbitrary SymbolicObj3 where
  shrink = filter isValid3 . genericShrink
  arbitrary = fmap (\x -> bool discard x $ isValid3 x) $ sized $ \n ->
    case n <= 1 of
      False -> oneof $
        [ rotate3  <$> arbitrary    <*> decayArbitrary 2
        , rotate3V <$> arbitrary    <*> arbitrary        <*> decayArbitrary 2
        , extrudeR <$> arbitraryPos <*> decayArbitrary 2 <*> arbitraryPos
        ] ++ arbitraryObject ++ small
      True -> oneof small
    where
      small =
        [ sphere    <$> arbitraryPos
        , cylinder  <$> arbitraryPos <*> arbitraryPos
        , cylinder2 <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
        , rect3R    <$> arbitraryPos <*> arbitrary    <*> arbitrary
        ]

instance Arbitrary ExtrudeRMScale where
  shrink = genericShrink
  arbitrary = oneof
    [ C1 <$> arbitrary
    , C2 <$> arbitrary
    , Fn <$> arbitrary
    ]


arbitraryObject :: (Arbitrary a, Arbitrary vec, Object a vec) => [Gen a]
arbitraryObject =
  [ translate   <$> arbitrary <*> decayArbitrary 2
  , I.scale     <$> arbitrary <*> decayArbitrary 2
  , unionR      <$> arbitraryPos <*> decayedList
  , intersectR  <$> arbitraryPos <*> decayedList
  , differenceR <$> arbitraryPos <*> decayedList
  , shell       <$> arbitraryPos <*> decayArbitrary 2
  ]


decayedList :: Arbitrary a => Gen [a]
decayedList = do
  n <- choose (1, 10)
  vectorOf n $ decayArbitrary $ n * 1

arbitraryPos :: Gen Double
arbitraryPos = quantize 3 . getPositive <$> arbitrary


decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

isValid2 :: SymbolicObj2 -> Bool
isValid2 (Complement2 s) = isValid2 s
isValid2 (UnionR2 _ []) = False
isValid2 (UnionR2 _ l_s) = all isValid2 l_s
isValid2 (DifferenceR2 _ l_s@(_:_:_)) = all isValid2 l_s
isValid2 (DifferenceR2 _ _) = False
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

isValid2 (RectR _ (x0, y0) (x1, y1)) = and
  [ x0 < x1
  , y0 < y1
  ]
isValid2 s =
  let (dx, dy) = boxSize s
   in not $ any (== 0) [dx, dy]

isValid3 :: SymbolicObj3 -> Bool
isValid3 (Complement3 s) = isValid3 s
isValid3 (UnionR3 _ []) = False
isValid3 (UnionR3 _ l_s) = all isValid3 l_s
isValid3 (DifferenceR3 _ l_s@(_:_:_)) = all isValid3 l_s
isValid3 (DifferenceR3 _ _) = False
isValid3 (IntersectR3 _ l_s@(_:_:_)) = all isValid3 l_s
isValid3 (IntersectR3 _ _) = False
isValid3 (Translate3 _ s) = isValid3 s
isValid3 (Scale3 (x, y, z) s) = x > 0 && y > 0 && z > 0 && isValid3 s
isValid3 (Rotate3 _ s) = isValid3 s
isValid3 (Rotate3V _ _ s) = isValid3 s
isValid3 (Outset3 _ s) = isValid3 s
isValid3 (Shell3 _ s) = isValid3 s
isValid3 (Rect3R _ (x0, y0, z0) (x1, y1, z1)) = and
  [ x0 < x1
  , y0 < y1
  , z0 < z1
  ]
isValid3 (Cylinder _ r h) = r > 0 && h > 0
isValid3 s =
  let (dx, dy, dz) = boxSize s
   in not $ any (== 0) [dx, dy, dz]

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

test :: IO ()
test = quickCheck $ \r (Small x1) (Small x2) (Small y1) (Small y2) (Small z1) (Small z2) ->
  let [x1', x2'] = fmap fromInteger $ sort [x1, x2]
      [y1', y2'] = fmap fromInteger $ sort [y1, y2]
      [z1', z2'] = fmap fromInteger $ sort [z1, z2]
      origin = (x1', y1', z1')
   in rect3R r origin (x2', y2', z2')
  =~= translate origin (rect3R r (0, 0, 0) (x2' - x1', y2' - y1', y2' - z1'))

test2 :: IO ()
test2 = quickCheck $ \obj -> within (1e6) $
      obj
  =~= rotate3 (2 * pi, 0, 0) obj

