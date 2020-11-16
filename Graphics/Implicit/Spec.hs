{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans              #-}

module Graphics.Implicit.Spec where

import qualified Graphics.Implicit as I
import           Graphics.Implicit hiding (scale)
import           Graphics.Implicit.Definitions (Polyline, TriangleMesh)
import           Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable(discreteAprox))
import           Graphics.Implicit.Primitives hiding (scale)
import           Prelude
import           QuickSpec
import           Test.QuickCheck


instance Arbitrary SymbolicObj2 where
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


instance Observe () TriangleMesh SymbolicObj3 where
  observe _ obj =
    discreteAprox 1 $
      implicit @SymbolicObj3
        (getImplicit obj)
        ((0, 0, 0), (1, 1, 1))

instance Observe () [Polyline] SymbolicObj2 where
  observe _ = discreteAprox 1


instance Arbitrary SymbolicObj3 where
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
        [ sphere    <$> arbitraryPos
        , cylinder  <$> arbitraryPos <*> arbitraryPos
        , cylinder2 <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
        , rect3R    <$> arbitraryPos <*> arbitrary    <*> arbitrary
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
  vectorOf n $ decayArbitrary $ n + 1

arbitraryPos :: Gen Double
arbitraryPos = getPositive <$> arbitrary


decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


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

