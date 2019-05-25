-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Rational, arbitrary precision trig functions.

-- Required. FIXME: why?
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeApplications #-}

module Graphics.Implicit.RationalUtil (ℚ(..), ℝ, fromFastℕtoℝ, fromℕtoℝ, fromℝtoℕ, fromℝtoFloat) where

import Prelude (RealFrac(properFraction), Fractional(fromRational, (/)), Ord, Double, Show(show), Eq, Num((+), (*), abs, negate, signum, fromInteger), Real(toRational), Read(readsPrec), ($), seq, (==), floor, Float, map)

import qualified Prelude as P ((+), (-), (*), abs, signum, negate, (/), (**), realToFrac, sqrt, cos, sin, tan, asin, acos, atan, sinh, cosh, tanh, atan2, pi, exp, log, fromIntegral, toRational, properFraction, show, fromRational, readsPrec)

import Data.AffineSpace (AffineSpace(Diff, (.-.), (.+^)))

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Implicit.IntegralUtil (ℕ)

import Graphics.Implicit.FastIntUtil (Fastℕ)

import Data.VectorSpace (InnerSpace((<.>)), AdditiveGroup((^+^), (^-^), zeroV, negateV), VectorSpace(Scalar,(*^)), (^/), magnitude)

import Control.DeepSeq(NFData(rnf))

import qualified Control.DeepSeq as CDS (rnf)


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

newtype ℝ = ℝ Double
  deriving (Ord, Eq)

-- Use this instance when ℝ ~ Double
instance ℚ ℝ where
  π = ℝ $ P.pi
  minℝ = ℝ $ 0.0000000000000002
  -- yes, these are nonsense. never meant to be evaluated.
  infty = ℝ $ 1/0
  neginfty = ℝ $ -1/0
  powℝ (ℝ a) b = ℝ $ a P.** (P.fromIntegral b)
  powℝℝ (ℝ a) (ℝ b) = ℝ $ a P.** b
  sin (ℝ x) = ℝ $ P.sin x
  cos (ℝ x) = ℝ $ P.cos x
  tan (ℝ x) = ℝ $ P.tan x
  asin (ℝ x) = ℝ $ P.asin x
  acos (ℝ x) = ℝ $ P.acos x
  atan (ℝ x) = ℝ $ P.atan x
  sinh (ℝ x) = ℝ $ P.sinh x
  cosh (ℝ x) = ℝ $ P.cosh x
  tanh (ℝ x) = ℝ $ P.tanh x
  atan2 (ℝ x) (ℝ y) = ℝ $ P.atan2 x y
  exp (ℝ x) = ℝ $ P.exp x
  log (ℝ x) = ℝ $ P.log x
  sqrt (ℝ x) = ℝ $ P.sqrt x
  cbrt (ℝ x) = ℝ $ (P.**(1/3)) x
  normalizeℝ2 (ℝ x, ℝ y) = bothℝ $ (x, y) ^/ magnitude (x, y)
    where bothℝ (a, b) = (ℝ a, ℝ b)
  normalizeℝ3 (ℝ x, ℝ y, ℝ z) = allThreeℝ $ (x, y, z) ^/ magnitude (x, y, z)
    where allThreeℝ (a, b, c) = (ℝ a, ℝ b, ℝ c)
  fromℝ (ℝ x) = P.realToFrac x
  toℝ a = a
  (%) a b = ℝ $ (P./) (P.fromIntegral a) (P.fromIntegral b)

instance Read ℝ where
  readsPrec prec input = map promoteFst $ (P.readsPrec prec input)
    where
      promoteFst :: (Double, a) -> (ℝ, a)
      promoteFst (q, r) = (ℝ q, r)
--  readsListPrec
--  readsList

instance Show ℝ where
    show (ℝ a) = P.show a

instance RealFrac ℝ where
  properFraction (ℝ a) = promoteSnd $ P.properFraction a
    where
      promoteSnd :: (a, Double) -> (a, ℝ)
      promoteSnd (q, r) = (q, ℝ r)
  -- ceiling, floor, truncate, round.

instance Fractional ℝ where
  fromRational x  = ℝ $ P.fromRational x
  (/) (ℝ x) (ℝ y) = ℝ $ (P./) x y
  
instance Real ℝ where
  toRational (ℝ a) = P.toRational a
  {-# INLINABLE toRational #-}

instance Num ℝ where
  (+) (ℝ a) (ℝ b) = ℝ $ (P.+) a b
  (*) (ℝ a) (ℝ b) = ℝ $ (P.*) a b
  abs (ℝ a)       = ℝ $ P.abs a
  negate (ℝ a)    = ℝ $ P.negate a
  signum (ℝ a)    = ℝ $ P.signum a
  fromInteger a   = ℝ $ P.realToFrac a

fromℕtoℝ :: ℕ -> ℝ
fromℕtoℝ a = ℝ $ P.realToFrac a

fromFastℕtoℝ :: Fastℕ -> ℝ
fromFastℕtoℝ a = ℝ $ P.realToFrac a

--fromInttoℝ :: Int -> ℝ
--fromInttoℝ a = ℝ $ P.realToFrac a

fromℝtoFloat :: ℝ -> Float
fromℝtoFloat a = (P.realToFrac a :: Float)

fromℝtoℕ :: ℝ -> Maybe ℕ
fromℝtoℕ n = if n == fromℕtoℝ (floor n) then Just (floor n) else Nothing

instance AdditiveGroup ℝ where
  zeroV             = ℝ $ 0
  (^-^) (ℝ a) (ℝ b) = ℝ $ (P.-) a b
  (^+^) (ℝ a) (ℝ b) = ℝ $ (P.+) a b
  negateV (ℝ a)     = ℝ $ P.negate a

instance NFData ℝ where
  rnf (ℝ a) = CDS.rnf a `seq` ()

instance VectorSpace ℝ where
  type Scalar ℝ = ℝ
  (*^) (ℝ a) (ℝ b) = ℝ $ (P.*) a b

instance InnerSpace ℝ where
  (<.>) = (P.*)

instance AffineSpace ℝ where
  type Diff ℝ = ℝ
  (.-.) = (P.-)
  (.+^) = (P.+)



