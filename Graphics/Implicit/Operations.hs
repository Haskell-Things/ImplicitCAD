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
	outset,
	shell,
	extrudeR, extrudeRMod,
	extrudeOnEdgeOf,
	rotate3
) where

-- classes in here provide basicaly everything we're exporting...
import Graphics.Implicit.Operations.Definitions

-- Then we have a bunch of isntances, corresponding to each file name.
import Graphics.Implicit.Operations.Obj2
import Graphics.Implicit.Operations.Obj3
import Graphics.Implicit.Operations.ObjPair
import Graphics.Implicit.Operations.BoxedObj2
import Graphics.Implicit.Operations.BoxedObj3
import Graphics.Implicit.Operations.BoxedObjPair
import Graphics.Implicit.Operations.SymbolicObj2
import Graphics.Implicit.Operations.SymbolicObj3
import Graphics.Implicit.Operations.SymbolicObjPair



{- Old stuff that may need to be incorporated into the larger structure later

-- | Slice a 3D objects at a given z value to make a 2D object.
slice :: 
	ℝ         -- ^ z-level to cut at
	-> Obj3   -- ^ 3D object to slice from
	-> Obj2   -- ^ Resulting 2D object
slice z obj = \(a,b) -> obj (a,b,z)


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


-}
