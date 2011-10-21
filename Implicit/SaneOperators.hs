-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}

-- We're going to be working with vectors, etc, a lot.
-- I'd rather not have to break every stupid vector into
-- its components to add them or scale them...

module Implicit.SaneOperators where

import qualified Prelude as P
import Prelude hiding ((+),(-),(*),(/))

import Implicit.Definitions

class Additive a b c | a b -> c where
	(+) :: a -> b -> c

class Multiplicative a b c | a b -> c where
	(*) :: a -> b -> c

class AdditiveInvertable a where
	additiveInverse :: a -> a

class MultiplicativeInvertable a where
	multiplicativeInverse :: a -> a

instance Num a => Additive a a a where
	a + b = a P.+ b

instance Num a => Multiplicative a a a where
	a * b = a P.* b

instance Num a => AdditiveInvertable a where
	additiveInverse a = negate a

instance Fractional a => MultiplicativeInvertable a where
	multiplicativeInverse a = 1 P./ a

(-) :: (Additive a b c) => (AdditiveInvertable b) => a -> b -> c
x - y = x + (additiveInverse y)

(/) :: (Multiplicative a b c) => (MultiplicativeInvertable b) => a -> b -> c
x / y = x * (multiplicativeInverse y)



instance Additive ℝ2 ℝ2 ℝ2 where
	(x1, y1) + (x2, y2) = (x1+y1, x2+y2)

instance Additive ℝ3 ℝ3 ℝ3 where
	(x1, y1, z1) + (x2, y2, z2) = (x1+y1, x2+y2, z1+z2)

instance Multiplicative ℝ ℝ2 ℝ2 where
	s * (x,y) = (s*x, s*y)

instance Multiplicative ℝ ℝ3 ℝ3 where
	s * (x,y,z) = (s*x, s*y, s*z)


instance (Additive a b c) => Additive (d -> a) (d -> b) (d -> c) where
	f + g = \p -> f p + g p

instance (Multiplicative a b c) => Multiplicative (d -> a) (d -> b) (d -> c) where
	f * g = \p -> f p * g p




