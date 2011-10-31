-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Graphics.Implicit.Operations (
	translate, 
	scale,
	complement,
	union,  intersect,  difference,
	unionR, intersectR, differenceR,
	unionL, intersectL, differenceL,
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


-- | Translate an object by a vector of appropriate dimension. 
translate :: 
	(Additive a a a, AdditiveInvertable a)
	=> a             -- ^ Vector to translate by (Also: a is a vector, blah, blah)
	-> (a -> ℝ)   -- ^ Object to translate
	-> (a -> ℝ)   -- ^ Resulting object
translate p obj = \q -> obj (q-p)

-- | Scale an object
scale :: (Multiplicative a ℝ a) => 
	ℝ            -- ^ Amount to scale by
	-> (a -> ℝ)  -- ^ Object to scale
	-> (a -> ℝ)  -- ^ Resulting scaled object
scale s obj = \p -> s * obj (p/s)

complement :: 
	(a -> ℝ)     -- ^ Object to complement
	-> (a -> ℝ)  -- ^ Result
complement obj = \p -> - obj p

shell :: 
	ℝ             -- ^ width of shell
	-> (a -> ℝ)   -- ^ object to take shell of
	-> (a -> ℝ)   -- ^ resulting shell
shell w a = \p -> abs (a p) - w/(2.0::ℝ)

-- | Rounded union
unionR :: 
	ℝ  -- ^ The radius of rounding
	-> (a -> ℝ) -- ^ First object to union
	-> (a -> ℝ) -- ^ Second object to union
	-> (a -> ℝ) -- ^ Resulting object
unionR r a b = \p -> rmin r (a p) (b p)

-- | Rounded minimum
intersectR :: ℝ  -- ^ The radius of rounding
	-> (a -> ℝ) -- ^ First object to intersect
	-> (a -> ℝ) -- ^ Second object to intersect
	-> (a -> ℝ) -- ^ Resulting object
intersectR r a b = \p -> rmax r (a p) (b p)

-- | Rounded difference
differenceR :: ℝ  -- ^ The radius of rounding
	-> (a -> ℝ) -- ^ First object 
	-> (a -> ℝ) -- ^ Object to cut out of the first object
	-> (a -> ℝ) -- ^ Resulting object
differenceR r a b = \p -> rmax r (a p) (- b p)


-- | Union a list of objects
unionL :: 
	[a -> ℝ] -- ^ List of objects to union
	-> (a -> ℝ) -- ^ The object resulting from the union
unionL objs = \p -> minimum $ map ($p) objs

-- | Intersect a list of objects
intersectL :: 
	[a -> ℝ] -- ^ List of objects to intersect
	-> (a -> ℝ) -- ^ The object resulting from the intersection
intersectL objs = \p -> maximum $ map ($p) objs

-- | Difference a list of objects
differenceL :: 
	[a -> ℝ] -- ^ List of objects to difference
	-> (a -> ℝ) -- ^ The object resulting from the difference
differenceL (obj:objs) = \p -> maximum $ map ($p) $ obj:(map complement objs)




union a b = unionL [a,b]
(∪) a b = union a b
intersect a b = intersectL [a,b]
(∩) a b = intersect a b
difference a b = differenceL [a,b]

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

