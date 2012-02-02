-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.Obj3 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.Primitives.Definitions

instance PrimitiveSupporter3 Obj3 where
	sphere r = \(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r
	rect3R r (x1,y1,z1) (x2,y2,z2) = 
		\(x,y,z) -> rmaximum r
			[abs (x-dx/2.0-x1) - dx/2.0, abs (y-dy/2.0-y1) - dy/2.0, abs (z-dz/2.0-z1) - dz/2.0]
				where (dx, dy, dz) = (x2-x1, y2-y1, z2-z1)
	cylinder2 r1 r2 h 
		| r1 == r2  = \(x,y,z) -> max (sqrt(x^2+y^2) - r1) (abs(z-h/2.0) - h/2.0)
		| otherwise = \(x,y,z) -> max (sqrt(x^2+y^2) - r1*(1.0 - z/2.0) - r2*z/2.0) (abs(z-h/2.0) - h/2.0)

