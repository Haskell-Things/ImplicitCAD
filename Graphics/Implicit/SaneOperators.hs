-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances  #-}

-- We're going to be working with vectors, etc, a lot.
-- I'd rather not have to break every stupid vector into
-- its components to add them or scale them...

module Graphics.Implicit.SaneOperators where

import qualified Prelude as P
import Prelude hiding ((+),(-),(*),(/))

import Graphics.Implicit.Definitions

-- * Num is too big a class and doesn't make sense for, say, vectors.

class Additive a b c | a b -> c where
	(+) :: a -> b -> c
	infixl 6 +

class Multiplicative a b c | a b -> c where
	(*) :: a -> b -> c
	infixl 7 *

class AdditiveInvertable a where
	additiveInverse :: a -> a

class MultiplicativeInvertable a where
	multiplicativeInverse :: a -> a

class Normable a where
	norm :: a -> ℝ

class InnerProductSpace a where
	(⋅) :: a -> a -> ℝ

-- * I should be able to create instances for all Num instances,
-- but Haskell's type checker doesn't seem to play nice with them.


{-instance Num a => Additive a a a where
	a + b = a P.+ b

instance Num a => Multiplicative a a a where
	a * b = a P.* b

instance Num a => AdditiveInvertable a where
	additiveInverse a = negate a

instance Fractional a => MultiplicativeInvertable a where
	multiplicativeInverse a = 1 P./ a-}

-- So, we do this instead. :(

instance Additive ℝ ℝ ℝ where
	a + b = a P.+ b

instance Multiplicative ℝ ℝ ℝ where
	a * b = a P.* b

instance AdditiveInvertable ℝ where
	additiveInverse a = negate a

instance MultiplicativeInvertable ℝ where
	multiplicativeInverse a = 1 P./ a

instance Additive ℕ ℕ ℕ where
	a + b = a P.+ b

instance Multiplicative ℕ ℕ ℕ where
	a * b = a P.* b

instance AdditiveInvertable ℕ where
	additiveInverse a = negate a


instance Additive ℝ ℕ ℝ where
	a + b = a P.+ (fromIntegral b)

instance Multiplicative ℝ ℕ ℝ where
	a * b = a P.* (fromIntegral b)

instance Additive ℕ ℝ ℝ where
	a + b = (fromIntegral a) P.+ b

instance Multiplicative ℕ ℝ ℝ where
	a * b = (fromIntegral a) P.* b


(-) :: (Additive a b c) => (AdditiveInvertable b) => a -> b -> c
x - y = x + (additiveInverse y)
infixl 6 -

(/) :: (Multiplicative a b c) => (MultiplicativeInvertable b) => a -> b -> c
x / y = x * (multiplicativeInverse y)
infixl 7 /



instance Additive ℝ2 ℝ2 ℝ2 where
	(x1, y1) + (x2, y2) = (x1+x2, y1+y2)

instance Additive ℝ3 ℝ3 ℝ3 where
	(x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

{-instance (Additive a b c, Additive d e f) => Additive (a,d) (b,e) (c,f) where
	(x1, y1) + (x2, y2) = (x1+x2, y1+y2)

instance (Additive a b c, Additive d e f, Additive  g h i) => Additive (a,d,g) (b,e,h) (c,f,i) where
	(x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)-}

instance Multiplicative ℝ ℝ2 ℝ2 where
	s * (x,y) = (s*x, s*y)

instance Multiplicative ℝ2 ℝ ℝ2 where
	(x,y)*s = (s*x, s*y)

instance Multiplicative ℝ ℝ3 ℝ3 where
	s * (x,y,z) = (s*x, s*y, s*z)

instance Multiplicative ℝ3 ℝ ℝ3 where
	(x,y,z) * s = (s*x, s*y, s*z)

instance AdditiveInvertable ℝ2 where
	additiveInverse (x, y) = (additiveInverse x, additiveInverse y)

instance AdditiveInvertable ℝ3 where
	additiveInverse (x, y, z) = (additiveInverse x, additiveInverse y, additiveInverse z)

{-instance (AdditiveInvertable a, AdditiveInvertable b) =>  AdditiveInvertable (a,b) where
	additiveInverse (x, y) = (additiveInverse x, additiveInverse y)

instance (AdditiveInvertable a, AdditiveInvertable b, AdditiveInvertable c) => AdditiveInvertable (a,b,c) where
	additiveInverse (x, y, z) = (additiveInverse x, additiveInverse y, additiveInverse z)-}



instance (Additive a b c) => Additive (d -> a) (d -> b) (d -> c) where
	f + g = \p -> f p + g p

instance (Multiplicative a b c) => Multiplicative (d -> a) (d -> b) (d -> c) where
	f * g = \p -> f p * g p


instance Normable ℝ where
	norm a = abs a

instance Normable ℝ2 where
	norm (a, b) = sqrt ((a**2) + (b**2))

instance Normable ℝ3 where
	norm (a, b, c) = sqrt ((a**2) + (b**2) + (c**2))

instance InnerProductSpace ℝ where
	x ⋅ y = x*y

instance InnerProductSpace ℝ2 where
	(a1, a2) ⋅ (b1, b2) = a1*b1 + a2*b2

instance InnerProductSpace ℝ3 where
	(a1, a2, a3) ⋅ (b1, b2, b3) = a1*b1 + a2*b2+a3*b3



