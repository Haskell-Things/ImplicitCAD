-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Rational, arbitrary precision trig functions.

-- Required. FIXME: why?
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE DeriveGeneric #-}

module Graphics.Implicit.RationalUtil (ℚ(..), ℝ, fromFastℕtoℝ, fromℕtoℝ, fromℝtoℕ, fromℝtoFloat) where

import Prelude ( (/), (-), fromIntegral, abs, realToFrac, RealFrac, Fractional, Ord, Double, (**), Show, Eq, Num((+), (*), negate, signum, fromInteger), Real, Read, ($), seq, (==), floor, Float)

import qualified Prelude as P ((/), (**), realToFrac, sqrt, cos, sin, tan, asin, acos, atan, sinh, cosh, tanh, atan2, pi, exp, log, fromIntegral)

import Data.Coerce (coerce)

import Data.AffineSpace (AffineSpace(Diff, (.-.), (.+^)))

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Implicit.IntegralUtil (ℕ)

import Graphics.Implicit.FastIntUtil (Fastℕ)

import Data.VectorSpace (InnerSpace((<.>)), AdditiveGroup, VectorSpace(Scalar,(*^)), (^/), magnitude)

import Control.DeepSeq(NFData(rnf))

-- import GHC.Generics (Generic)

-- FIXME: this looks a lot like VectorSpace. Make ScadSpace?
-- Properties of ScadSpace:
-- It's own Cos, Sin, and Arctan2 implementations.
-- It's own power operator.
-- It's own stopping point when trying to decide what side of a line a ray is on.
-- One implementation for float, one for Double, and one for Ratio ℕ.

-- Let's make things a bit nicer. 
-- Supports using Float, Double, or a Ratio of ℕ for more precision!
class (RealFrac v) => ℚ v where
  π :: v
  minℝ :: v
  infty :: v
  neginfty :: v
  powℝ :: v -> ℕ -> v
  powℝℝ :: v -> v -> v
  sin :: v -> v
  cos :: v -> v
  tan :: v -> v
  asin :: v -> v
  acos :: v -> v
  atan :: v -> v
  sinh :: v -> v
  cosh :: v -> v
  tanh :: v -> v
  exp :: v -> v
  log :: v -> v
  sqrt :: v -> v
  cbrt :: v -> v
  atan2 :: v -> v -> v
  normalizeℝ :: v -> v
  normalizeℝ2 :: (v,v) -> (v,v)
  normalizeℝ3 :: (v,v,v) -> (v,v,v)
  toℝ :: v -> ℝ
  fromℝ :: ℝ -> v
  (%) :: ℕ -> ℕ -> v

-- CUT HERE --
{-

import GHC.Real (Ratio((:%)))

import Graphics.Implicit.RationalFunctions (cosℝp, sinℝp, asinℝp, normalizeℝp, normalizeℝ2p, normalizeℝ3p, sqrtℝp, cbrtℝp)

import Prelude (Integer, (^^), Real(toRational))

import Graphics.Implicit.IntegralUtil (Fastℕ, ℕ, toℕ, fromℕ)

-- Double Precision sqrt is 53 bits.
sqrtPrecision :: Fastℕ
sqrtPrecision=53

-- FIXME: find a better representation of the default precision of the trig functions.

instance ℚ (Ratio ℕ) where
  π = 245850922%78256779
  -- FIXME: placeholders.
  minℝ = 1%10000000000000000
  infty=10000000000000000%1
  neginfty=(-10000000000000000%1)
  powℝ a b =  a ^^ b
  sin = sinℝp sqrtPrecision
  cos = cosℝp sqrtPrecision
--  tan = tanℝp sqrtPrecision
  asin = asinℝp sqrtPrecision
  acos = acosℝp sqrtPrecision
--  atan = atanℝp sqrtPrecision
--  sinh = sinhℝp sqrtPrecision
--  cosh = coshℝp sqrtPrecision
--  tanh = tanhℝp sqrtPrecision
  sqrt = sqrtℝp sqrtPrecision
  cbrt = cbrtℝp sqrtPrecision
--  atan2 = atan2ℝp sqrtPrecision
  normalizeℝ = normalizeℝp sqrtPrecision
  normalizeℝ2 = normalizeℝ2p sqrtPrecision
  normalizeℝ3 = normalizeℝ3p sqrtPrecision
--  fromℝ (ℝ (a :% b)) = (fromIntegral (fromℕ a :: Integer)) :% (fromIntegral (fromℕ b :: Integer))
  fromℝ (ℝ (a :% b)) = (fromℕ a) :% (fromℕ b)
  toℝ (a :% b) =  ℝ ((toℕ a) :% (toℕ b))
  (%) a b = a :% b

newtype ℝ = ℝ (Ratio ℕ)
  deriving stock (Read, Show, Ord, Eq)
  deriving newtype (RealFrac, Fractional, ℚ, AdditiveGroup, AffineSpace)

instance NFData ℝ where
  rnf (ℝ (x :% y)) = rnf (fromℕ x :: Integer) `seq`
                     rnf (fromℕ y :: Integer) `seq`
                     ()

instance Num ℝ where
  (+) (ℝ a) (ℝ b) = ℝ $ a + b
  (*) (ℝ a) (ℝ b) = ℝ $ a * b
  abs (ℝ a) = ℝ $ abs a
  negate (ℝ a) = ℝ $ negate a
  signum (ℝ a) = ℝ $ signum a
  fromInteger a = ℝ $ (toℕ a):%1

instance Real ℝ where
  toRational (ℝ (a :% b)) = (fromℕ a) :% (fromℕ b)

fromℕtoℝ :: ℕ -> ℝ
fromℕtoℝ a = ℝ $ a % (1::ℕ)

acosℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
acosℝp precision x = (π/2)-(asinℝp precision (abs x))

-}
-- CUT HERE --

instance ℚ Double where
  π = P.pi
  minℝ = 0.000000000000002
  -- yes, these are nonsense. never meant to be evaluated.
  infty = 1/0
  neginfty = -1/0
  powℝ a b = a P.** (P.fromIntegral b)
  powℝℝ a b = a P.** b
  sin = P.sin
  cos = P.cos
  tan = P.tan
  asin = P.asin
  acos = P.acos
  atan = P.atan
  sinh = P.sinh
  cosh = P.cosh
  tanh = P.tanh
  atan2 = P.atan2
  exp = P.exp
  log = P.log
  sqrt = P.sqrt
  cbrt = (**(1/3))
  normalizeℝ v = v ^/ magnitude v 
  normalizeℝ2 v = v ^/ magnitude v 
  normalizeℝ3 v = v ^/ magnitude v 
  fromℝ = P.realToFrac
  toℝ a = ℝ a
  (%) a b = (P./) (P.fromIntegral a) (P.fromIntegral b)

newtype ℝ = ℝ Double
--  deriving (Read, Show, Ord, Eq, RealFrac, Fractional, Generic, ℚ, AdditiveGroup, Real)
  deriving stock (Show, Ord, Eq)
  deriving newtype (Read, RealFrac, Fractional, ℚ, AdditiveGroup, Real)

instance NFData ℝ where
  rnf (ℝ a) = rnf a `seq` ()

instance Num ℝ where
  (+) (ℝ a) (ℝ b) = ℝ $ a + b
  (*) (ℝ a) (ℝ b) = ℝ $ a * b
  abs (ℝ a) = ℝ $ abs a
  negate (ℝ a) = ℝ $ negate a
  signum (ℝ a) = ℝ $ signum a
  fromInteger a = ℝ $ realToFrac a

fromℕtoℝ :: ℕ -> ℝ
fromℕtoℝ a = ℝ $ realToFrac a

fromFastℕtoℝ :: Fastℕ -> ℝ
fromFastℕtoℝ a = ℝ $ realToFrac a

--fromInttoℝ :: Int -> ℝ
--fromInttoℝ a = ℝ $ realToFrac a

fromℝtoFloat :: ℝ -> Float
fromℝtoFloat a = (realToFrac a :: Float)

-- CUT HERE --

{-
import Prelude (Float)

instance ℚ Float where
  π = P.pi
  minℝ = 0.00000002
  -- yes, these are nonsense. never meant to be evaluated.
  infty = 1/0
  neginfty = -1/0
  powℝ a b = a ** (fromIntegral b)
  cos = P.cos
  sin = P.sin
  acos = P.acos
  asin = P.asin
  sqrt = P.sqrt
  normalizeℝ v = v ^/ magnitude v 
  fromℝ = realToFrac
  toℝ a = ℝ a
newtype ℝ = ℝ Float
-}

-- Thanks, Solonarv and #haskell
-- copied from the VectorSpace (Ratio a) instance
instance VectorSpace ℝ where
  type Scalar ℝ = ℝ 
  (*^) = coerce ((*^) @(ℝ))

instance InnerSpace ℝ where
  (<.>) = (*)

instance AffineSpace ℝ where
  type Diff ℝ = ℝ
  (.-.) = (-)
  (.+^) = (+)


fromℝtoℕ :: ℝ -> Maybe ℕ
fromℝtoℕ n = if n == fromℕtoℝ (floor n) then Just (floor n) else Nothing


