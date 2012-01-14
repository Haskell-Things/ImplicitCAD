-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations (
	translate, 
	scale,
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

-- If you are confused as to how these functions work, please refer to
-- http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

infty = (1 :: ℝ) / (0 :: ℝ)

-- | Very basic operations objects
class BasicObj obj vec | obj -> vec where
	
	-- | Translate an object by a vector of appropriate dimension. 
	translate :: 
		vec      -- ^ Vector to translate by (Also: a is a vector, blah, blah)
		-> obj   -- ^ Object to translate
		-> obj   -- ^ Resulting object

	-- | Scale an object
	scale :: 
		ℝ       -- ^ Amount to scale by
		-> obj  -- ^ Object to scale
		-> obj  -- ^ Resulting scaled object
	
	-- | Complement an Object
	complement :: 
		obj     -- ^ Object to complement
		-> obj  -- ^ Result
	
	-- | Union a list of objects
	union :: 
		[obj]  -- ^ List of objects to union
		-> obj -- ^ The object resulting from the union

	-- | Difference a list of objects
	difference :: 
		[obj]  -- ^ List of objects to difference
		-> obj -- ^ The object resulting from the difference
	
	-- | Intersect a list of objects
	intersect :: 
		[obj]  -- ^ List of objects to intersect
		-> obj -- ^ The object resulting from the intersection


instance BasicObj Obj2 ℝ2 where
	translate p obj = \q -> obj (q-p)
	scale s obj = \p -> s * obj (p/s)
	complement obj = \p -> - obj p
	union objs = \p -> minimum $ map ($p) objs
	intersect objs = \p -> maximum $ map ($p) objs
	difference (obj:objs) = \p -> maximum $ map ($p) $ obj:(map complement objs)


instance BasicObj Obj3 ℝ3 where
	translate p obj = \q -> obj (q-p)
	scale s obj = \p -> s * obj (p/s)
	complement obj = \p -> - obj p
	union objs = \p -> minimum $ map ($p) objs
	intersect objs = \p -> maximum $ map ($p) objs
	difference (obj:objs) = \p -> maximum $ map ($p) $ obj:(map complement objs)

-- CSG on 2D boxes
-- Not precise, since not all CSG of such is a 2D box, 
-- but result will be a super set. We will use this for bounding boxes.
-- Empty boxes will always be ((0,0),(0,0)) for convenience :)
instance BasicObj Box2 ℝ2 where
	translate _ ((0,0),(0,0)) = ((0,0),(0,0))
	translate p (a,b) = (a+p, b+p)
	scale s (a,b) = (s*a, s*b)
	complement _ = ((-infty, -infty), (infty, infty))
	union boxes = ((left,bot),(right,top)) where
			isEmpty = ( == ((0,0),(0,0)) )
			(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
			(lefts, bots) = unzip leftbot
			(rights, tops) = unzip topright
			left = minimum lefts
			bot = minimum bots
			right = maximum rights
			top = maximum tops
	intersect boxes = 
		let
			(leftbot, topright) = unzip boxes
			(lefts, bots) = unzip leftbot
			(rights, tops) = unzip topright
			left = maximum lefts
			bot = maximum bots
			right = minimum rights
			top = minimum tops
		in
			if top > bot && right > left 
			then ((left,bot),(right,top))
			else ((0,0),(0,0))
	difference (firstBox : otherBoxes) = firstBox

instance BasicObj Box3 ℝ3 where
	translate _ ((0,0,0),(0,0,0)) = ((0,0,0),(0,0,0))
	translate p (a,b) = (a+p, b+p)
	scale s (a,b) = (s*a, s*b)
	complement _ = ((-infty, -infty,-infty), (infty, infty, infty))
	union boxes = ((left,bot,inward),(right,top,out)) where
			isEmpty = ( == ((0,0,0),(0,0,0)) )
			(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
			(lefts, bots, ins) = unzip3 leftbot
			(rights, tops, outs) = unzip3 topright
			left = minimum lefts
			bot = minimum bots
			inward = minimum ins
			right = maximum rights
			top = maximum tops
			out = maximum outs
	intersect boxes = 
		let
			(leftbot, topright) = unzip boxes
			(lefts, bots, ins) = unzip3 leftbot
			(rights, tops, outs) = unzip3 topright
			left = maximum lefts
			bot = maximum bots
			inward = maximum ins
			right = minimum rights
			top = minimum tops
			out = minimum outs
		in
			if top > bot && right > left && out > inward
			then ((left,bot,inward),(right,top,out))
			else ((0,0,0),(0,0,0))
	difference (firstBox : otherBoxes) = firstBox


instance BasicObj (Boxed2 Obj2) ℝ2 where
	translate p (obj, box) = (translate p obj, translate p box)
	scale s (obj, box) = (scale s obj, scale s box)
	complement (obj, box) = (complement obj, complement box )
	union bobjs = (union objs, union boxes) where
		(objs, boxes) = unzip bobjs
	intersect bobjs = (intersect objs, intersect boxes) where
		(objs, boxes) = unzip bobjs
	difference bobjs = (difference objs, difference boxes) where
		(objs, boxes) = unzip bobjs

instance BasicObj (Boxed3 Obj3) ℝ3 where
	translate p (obj, box) = (translate p obj, translate p box)
	scale s (obj, box) = (scale s obj, scale s box)
	complement (obj, box) = (complement obj, complement box )
	union bobjs = (union objs, union boxes) where
		(objs, boxes) = unzip bobjs
	intersect bobjs = (intersect objs, intersect boxes) where
		(objs, boxes) = unzip bobjs
	difference bobjs = (difference objs, difference boxes) where
		(objs, boxes) = unzip bobjs


class MagnitudeObj obj where

	-- | Outset an object.
	outset :: 
		ℝ        -- ^ distance to outset
		-> obj   -- ^ object to outset
		-> obj   -- ^ resulting object

	-- | Make a shell of an object.
	shell :: 
		ℝ        -- ^ width of shell
		-> obj   -- ^ object to take shell of
		-> obj   -- ^ resulting shell
	
	-- | Rounded union
	unionR :: 
		ℝ        -- ^ The radius of rounding
		-> [obj] -- ^ objects to union
		-> obj   -- ^ Resulting object
	
	-- | Rounded minimum
	intersectR :: 
		ℝ        -- ^ The radius of rounding
		-> [obj] -- ^ Objects to intersect
		-> obj   -- ^ Resulting object
	
	-- | Rounded difference
	differenceR :: 
		ℝ        -- ^ The radius of rounding
		-> [obj] -- ^ Objects to difference 
		-> obj   -- ^ Resulting object


-- | Inset an object.
inset :: MagnitudeObj obj =>
	ℝ        -- ^ distance to inset
	-> obj   -- ^ object to inset
	-> obj   -- ^ resulting object
inset d obj = outset (-d) obj


instance MagnitudeObj Obj2 where
	outset d obj = \p -> obj p - d
	shell w a = \p -> abs (a p) - w/(2.0::ℝ)
	unionR r objs = \p -> rminimum r $ map ($p) objs
	intersectR r objs = \p -> rmaximum r $ map ($p) objs
	differenceR r (x:xs) = \p -> rmaximum r $ (x p) :(map (negate . ($p)) xs)

instance MagnitudeObj Obj3 where
	outset d obj = \p -> obj p - d
	shell w a = \p -> abs (a p) - w/(2.0::ℝ)
	unionR r objs = \p -> rminimum r $ map ($p) objs
	intersectR r objs = \p -> rmaximum r $ map ($p) objs
	differenceR r (x:xs) = \p -> rmaximum r $ (x p) :(map (negate . ($p)) xs)

instance MagnitudeObj Box2 where
	outset d (a,b) = (a - (d,d), b + (d,d))
	shell w (a,b) = (a - (w/(2.0::ℝ),w/(2.0::ℝ)), b + (w/(2.0::ℝ),w/(2.0::ℝ)))
	unionR r boxes = outset r $ union boxes
	intersectR r boxes = outset r $ intersect boxes
	differenceR r boxes = outset r $ difference boxes

instance MagnitudeObj Box3 where
	outset d (a,b) = (a - (d,d,d), b + (d,d,d))
	shell w (a,b) = (a - (w/(2.0::ℝ),w/(2.0::ℝ),w/(2.0::ℝ)), b + (w/(2.0::ℝ),w/(2.0::ℝ),w/(2.0::ℝ)))
	unionR r boxes = outset r $ union boxes
	intersectR r boxes = outset r $ intersect boxes
	differenceR r boxes = outset r $ difference boxes

instance MagnitudeObj (Boxed2 Obj2) where
	outset  d     (obj, box) = (outset d obj, outset d box)
	shell   w     (obj, box) = (shell  w obj, shell  w box)
	unionR      r bobjs      = (unionR      r objs, unionR      r boxes) where
		(objs, boxes) = unzip bobjs
	intersectR  r bobjs      = (intersectR  r objs, intersectR  r boxes) where
		(objs, boxes) = unzip bobjs
	differenceR r bobjs      = (differenceR r objs, differenceR r boxes) where
		(objs, boxes) = unzip bobjs

instance MagnitudeObj (Boxed3 Obj3) where
	outset  d     (obj, box) = (outset d obj, outset d box)
	shell   w     (obj, box) = (shell  w obj, shell  w box)
	unionR      r bobjs      = (unionR      r objs, unionR      r boxes) where
		(objs, boxes) = unzip bobjs
	intersectR  r bobjs      = (intersectR  r objs, intersectR  r boxes) where
		(objs, boxes) = unzip bobjs
	differenceR r bobjs      = (differenceR r objs, differenceR r boxes) where
		(objs, boxes) = unzip bobjs

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


