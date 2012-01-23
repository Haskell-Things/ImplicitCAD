-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations (
	BasicObj, MagnitudeObj,
	translate, 
	scale,
	rotateXY,
	complement,
	union,  intersect,  difference, 
	unionR,  intersectR,  differenceR, 
	shell,
	slice,
	bubble,
	extrude,
	extrudeR,
	extrudeOnEdgeOf
) where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators

import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Operations.Obj2
import Graphics.Implicit.Operations.Obj3
import Graphics.Implicit.Operations.Box2
import Graphics.Implicit.Operations.Box3
import Graphics.Implicit.Operations.BoxedObj2
import Graphics.Implicit.Operations.BoxedObj3


-- If you are confused as to how these functions work, please refer to
-- http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/




-- | Slice a 3D objects at a given z value to make a 2D object.
slice :: 
	ℝ         -- ^ z-level to cut at
	-> Obj3   -- ^ 3D object to slice from
	-> Obj2   -- ^ Resulting 2D object
slice z obj = \(a,b) -> obj (a,b,z)

-- | Bubble out a 2D object into a 3D one.
bubble :: ℝ -> Obj2 -> Obj3
bubble s obj = 
	let
		spsqrt n = signum n * sqrt (abs n)
		spsq n = signum n * n ** 2
	in
		\(x,y,z) -> spsqrt ( z ** 2 + s * obj (x,y) )

-- | Extrude a 2D object. (The extrusion goes into the z-plane)
extrude :: 
	ℝ          -- ^ Length to extrude
	-> Obj2    -- ^ 2D object to extrude
	-> Obj3    -- ^ Resulting 3D object
extrude h obj = \(x,y,z) -> max (obj (x,y)) (abs (z + h/(2.0 :: ℝ )) - h)

-- | Rounded extrude. Instead of the extrude having a flat top or bottom, it is bevelled.
extrudeR ::
	ℝ          -- ^ Radius of rounding
	-> ℝ       -- ^ Length to extrude
	-> Obj2    -- ^ 2D object to extrude
	-> Obj3    -- ^ Resulting 3D object
extrudeR r h obj = \(x,y,z) -> rmax r (obj (x,y)) (abs (z + h/(2.0 :: ℝ)) - h)

-- | Create a 3D object by extruding a 2D object along the edge of another 2D object.
-- For example, extruding a circle on the edge of another circle would make a torus.
extrudeOnEdgeOf :: 
	Obj2     -- ^ Object to extrude
	-> Obj2  -- ^ Object to extrude along the edge of
	-> Obj3  -- ^ Resulting 3D object
extrudeOnEdgeOf a b = \(x,y,z) -> a (b (x,y), z) 























