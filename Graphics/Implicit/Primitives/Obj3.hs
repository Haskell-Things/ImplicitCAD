-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.Obj3 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives.Definitions

instance PrimitiveSupporter3 Obj3 where
	sphere r = \(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r
	cubeV (dx, dy, dz) = 
		\(x,y,z) -> (maximum [abs (x-dx/2.0) - dx/2.0, abs (y-dy/2.0) - dy/2.0, abs (z-dz/2.0) - dz/2.0])
	cylinder2 r1 r2 h 
		| r1 == r2  = \(x,y,z) -> max (sqrt(x^2+y^2) - r1) (abs(z-h/2.0) - h)
		| otherwise = \(x,y,z) -> max (sqrt(x^2+y^2) - r1*(1.0 - z/2.0) - r2*z/2.0) (abs(z-h/2.0) - h)
	torus r_main r_second = \(x,y,z) -> sqrt( ( sqrt (x^2 + y^2) - r_main )^2 + z^2 ) - r_second

