-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Operations.ObjPair where

import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.Operations.Definitions



instance PairObj Obj2 ℝ2 Obj3 ℝ3 where

	-- Notice that \(x,y,z) = obj2 (x,y) infinitly extrudes a obj2 in both directions.
	-- We essentially do that but rounded intersect it to get the desired height.
	extrudeR r obj h = \(x,y,z) -> rmax r (obj (x,y)) (abs (z - h/2.0) - h/2.0)

	-- As above, but (obj $ mod z (x,y)) to modify to the object over ehight :)
	extrudeRMod r mod obj h = \(x,y,z) -> rmax r (obj $ mod z (x,y)) (abs (z - h/2.0) - h/2.0)

	-- We feed the output of one object as an input to another.
	extrudeOnEdgeOf a b = \(x,y,z) -> a (b (x,y), z)

	rotate3 (yz, xz, xy) obj = 
		let
			rotateYZ θ obj = \(x,y,z) -> obj ( x, cos(θ)*z - sin(θ)*y, cos(θ)*y + sin(θ)*z)
			rotateXZ θ obj = \(x,y,z) -> obj ( cos(θ)*x + sin(θ)*z, y, cos(θ)*z - sin(θ)*x)
			rotateXY θ obj = \(x,y,z) -> obj ( cos(θ)*x + sin(θ)*y, cos(θ)*y - sin(θ)*x, z)
		in
			rotateYZ yz $ rotateXZ xz $ rotateXY xy $ obj

