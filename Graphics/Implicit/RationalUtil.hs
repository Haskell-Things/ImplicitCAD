-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Rational, arbitrary precision trig functions.

-- Required. FIXME: why?
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeApplications #-}

module Graphics.Implicit.RationalUtil (ℚ(..), ℝ) where

import Prelude (RealFrac(properFraction, ceiling, floor, round, truncate), Fractional(fromRational, (/)), Ord, Double, Show(show), Eq, Num((+), (*), abs, negate, signum, fromInteger), Real(toRational), Read(readsPrec), ($), seq, (==), floor, Float, map)

import qualified Prelude as P ((+), (-), (*), abs, signum, negate, (/), (**), realToFrac, sqrt, cos, sin, tan, asin, acos, atan, sinh, cosh, tanh, atan2, pi, exp, log, fromIntegral, toRational, properFraction, show, fromRational, readsPrec, ceiling, floor, round, truncate)

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
  toℝ :: v -> v
  fromℕtoℝ :: ℕ -> v
  fromFastℕtoℝ :: Fastℕ -> v
  fromℝ :: v -> v
  fromℝtoFloat :: v -> Float
  fromℝtoℕ :: v -> Maybe ℕ
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
  {-# INLINABLE π #-}
  minℝ = ℝ $ 0.0000000000000002
  {-# INLINABLE minℝ #-}
  -- yes, these are nonsense. never meant to be evaluated.
  infty = ℝ $ 1/0
  {-# INLINABLE infty #-}
  neginfty = ℝ $ -1/0
  {-# INLINABLE neginfty #-}
  powℝ (ℝ a) b = ℝ $ a P.** (P.fromIntegral b)
  {-# INLINABLE powℝ #-}
  powℝℝ (ℝ a) (ℝ b) = ℝ $ a P.** b
  {-# INLINABLE powℝℝ #-}
  sin (ℝ x) = ℝ $ P.sin x
  {-# INLINABLE sin #-}
  cos (ℝ x) = ℝ $ P.cos x
  {-# INLINABLE cos #-}
  tan (ℝ x) = ℝ $ P.tan x
  {-# INLINABLE tan #-}
  asin (ℝ x) = ℝ $ P.asin x
  {-# INLINABLE asin #-}
  acos (ℝ x) = ℝ $ P.acos x
  {-# INLINABLE acos #-}
  atan (ℝ x) = ℝ $ P.atan x
  {-# INLINABLE atan #-}
  sinh (ℝ x) = ℝ $ P.sinh x
  {-# INLINABLE sinh #-}
  cosh (ℝ x) = ℝ $ P.cosh x
  {-# INLINABLE cosh #-}
  tanh (ℝ x) = ℝ $ P.tanh x
  {-# INLINABLE tanh #-}
  atan2 (ℝ x) (ℝ y) = ℝ $ P.atan2 x y
  {-# INLINABLE atan2 #-}
  exp (ℝ x) = ℝ $ P.exp x
  {-# INLINABLE exp #-}
  log (ℝ x) = ℝ $ P.log x
  {-# INLINABLE log #-}
  sqrt (ℝ x) = ℝ $ P.sqrt x
  {-# INLINABLE sqrt #-}
  cbrt (ℝ x) = ℝ $ (P.**(1/3)) x
  {-# INLINABLE cbrt #-}
  normalizeℝ2 (ℝ x, ℝ y) = bothℝ $ (x, y) ^/ magnitude (x, y)
    where bothℝ (a, b) = (ℝ a, ℝ b)
  {-# INLINABLE normalizeℝ2 #-}
  normalizeℝ3 (ℝ x, ℝ y, ℝ z) = allThreeℝ $ (x, y, z) ^/ magnitude (x, y, z)
    where allThreeℝ (a, b, c) = (ℝ a, ℝ b, ℝ c)
  {-# INLINABLE normalizeℝ3 #-}
  toℝ a = a
  {-# INLINABLE toℝ #-}
  fromℕtoℝ a = ℝ $ P.realToFrac a
  {-# INLINABLE fromℕtoℝ #-}
  fromFastℕtoℝ a = ℝ $ P.realToFrac a
  {-# INLINABLE fromFastℕtoℝ #-}
  fromℝtoFloat a = (P.realToFrac a :: Float)
  {-# INLINABLE fromℝtoFloat #-}
  fromℝtoℕ n = if n == fromℕtoℝ (floor n) then Just (floor n) else Nothing
  {-# INLINABLE fromℝtoℕ #-}
  fromℝ (ℝ x) = P.realToFrac x
  {-# INLINABLE fromℝ #-}
  (%) a b = ℝ $ (P./) (P.fromIntegral a) (P.fromIntegral b)
  {-# INLINABLE (%) #-}

instance Read ℝ where
  readsPrec prec input = map promoteFst $ (P.readsPrec prec input)
    where
      promoteFst :: (Double, a) -> (ℝ, a)
      promoteFst (q, r) = (ℝ q, r)
  {-# INLINABLE readsPrec #-}
-- FIXME: implement:
--  readsListPrec
--  readsList

instance Show ℝ where
  show (ℝ a) = P.show a
  {-# INLINABLE show #-}

instance RealFrac ℝ where
  ceiling (ℝ a)        = P.ceiling a
  {-# INLINABLE ceiling #-}
  floor (ℝ a)          = P.floor a
  {-# INLINABLE floor #-}
  truncate (ℝ a)       = P.truncate a
  {-# INLINABLE truncate #-}
  round (ℝ a)          = P.round a
  {-# INLINABLE round #-}
  properFraction (ℝ a) = promoteSnd $ P.properFraction a
    where
      promoteSnd :: (a, Double) -> (a, ℝ)
      promoteSnd (q, r) = (q, ℝ r)
  {-# INLINABLE properFraction #-}

instance Fractional ℝ where
  fromRational x  = ℝ $ P.fromRational x
  {-# INLINABLE fromRational #-}
  (/) (ℝ x) (ℝ y) = ℝ $ (P./) x y
  {-# INLINABLE (/) #-}
  
instance Real ℝ where
  toRational (ℝ a) = P.toRational a
  {-# INLINABLE toRational #-}

instance Num ℝ where
  (+) (ℝ a) (ℝ b) = ℝ $ (P.+) a b
  {-# INLINABLE (+) #-}
  (*) (ℝ a) (ℝ b) = ℝ $ (P.*) a b
  {-# INLINABLE (*) #-}
  abs (ℝ a)       = ℝ $ P.abs a
  {-# INLINABLE abs #-}
  negate (ℝ a)    = ℝ $ P.negate a
  {-# INLINABLE negate #-}
  signum (ℝ a)    = ℝ $ P.signum a
  {-# INLINABLE signum #-}
  fromInteger a   = ℝ $ P.realToFrac a
  {-# INLINABLE fromInteger #-}

instance AdditiveGroup ℝ where
  zeroV             = ℝ $ 0
  {-# INLINABLE zeroV #-}
  (^-^) (ℝ a) (ℝ b) = ℝ $ (P.-) a b
  {-# INLINABLE (^-^) #-}
  (^+^) (ℝ a) (ℝ b) = ℝ $ (P.+) a b
  {-# INLINABLE (^+^) #-}
  negateV (ℝ a)     = ℝ $ P.negate a
  {-# INLINABLE negateV #-}

instance NFData ℝ where
  rnf (ℝ a) = CDS.rnf a `seq` ()
  {-# INLINABLE rnf #-}

instance VectorSpace ℝ where
  type Scalar ℝ = ℝ
  (*^) (ℝ a) (ℝ b) = ℝ $ (P.*) a b
  {-# INLINABLE (*^) #-}

instance InnerSpace ℝ where
  (<.>) = (P.*)
  {-# INLINABLE (<.>) #-}

instance AffineSpace ℝ where
  type Diff ℝ = ℝ
  (.-.) = (P.-)
  {-# INLINABLE (.-.) #-}
  (.+^) = (P.+)
  {-# INLINABLE (.+^) #-}



