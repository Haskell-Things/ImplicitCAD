-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, NoMonomorphismRestriction #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions
import Data.List (sortBy)
import Graphics.Implicit.MathUtil   (pack)
import Graphics.Implicit.ObjectUtil (getBox2, getBox3, getImplicit2, getImplicit3)
import Data.AffineSpace.Point

-- $ 3D Primitives

sphere ::
	ℝ                  -- ^ Radius of the sphere
	-> SymbolicObj3    -- ^ Resulting sphere

sphere r = Sphere r

rect3R ::
	ℝ                 -- ^ Rounding of corners
	-> 𝔼3             -- ^ Bottom.. corner
	-> 𝔼3             -- ^ Top right... corner
	-> SymbolicObj3   -- ^ Resuting cube - (0,0,0) is bottom left...

rect3R = Rect3R

cylinder2 ::
	ℝ                   -- ^ Radius of the cylinder	
	-> ℝ                -- ^ Second radius of the cylinder
	-> ℝ                -- ^ Height of the cylinder
	-> SymbolicObj3     -- ^ Resulting cylinder

cylinder2 r1 r2 h = Cylinder h r1 r2

cylinder r = cylinder2 r r

-- $ 2D Primitives

circle ::
	ℝ               -- ^ radius of the circle
	-> SymbolicObj2 -- ^ resulting circle

circle   = Circle

rectR ::
	ℝ
	-> 𝔼2           -- ^ Bottom left corner
	-> 𝔼2           -- ^ Top right corner
	-> SymbolicObj2 -- ^ Resulting square (bottom right = (0,0) )

rectR = RectR

polygonR ::
	ℝ                -- ^ Rouding of the polygon
	-> [𝔼2]          -- ^ Vertices of the polygon
	-> SymbolicObj2  -- ^ Resulting polygon

polygonR = PolygonR

polygon = polygonR 0

-- $ Shared Operations

class Object obj vec | obj -> vec where
	
	-- | Translate an object by a vector of appropriate dimension. 
	translate :: 
		vec      -- ^ Vector to translate by (Also: a is a vector, blah, blah)
		-> obj   -- ^ Object to translate
		-> obj   -- ^ Resulting object

	-- | Scale an object
	scale :: 
		vec     -- ^ Amount to scale by
		-> obj  -- ^ Object to scale
		-> obj  -- ^ Resulting scaled object	
	
	-- | Complement an Object
	complement :: 
		obj     -- ^ Object to complement
		-> obj  -- ^ Result
	
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

	-- | Get the bounding box an object
	getBox :: 
		obj                       -- ^ Object to get box of
		-> (Point vec, Point vec) -- ^ Bounding box

	-- | Get the implicit function for an object
	getImplicit :: 
		obj                 -- ^ Object to get implicit function of
		-> (Point vec -> ℝ) -- ^ Implicit function

	implicit :: 
		(Point vec -> ℝ)           -- ^ Implicit function
		-> (Point vec, Point vec)  -- ^ Bounding box
		-> obj                     -- ^ Resulting object
	

instance Object SymbolicObj2 ℝ2 where
	translate   = Translate2
	scale       = Scale2
	complement  = Complement2
	unionR      = UnionR2
	intersectR  = IntersectR2
	differenceR = DifferenceR2
	outset      = Outset2
	shell       = Shell2
	getBox      = getBox2
	getImplicit = getImplicit2
	implicit a b= EmbedBoxedObj2 (a,b)

instance Object SymbolicObj3 ℝ3 where
	translate   = Translate3
	scale       = Scale3
	complement  = Complement3
	unionR      = UnionR3
	intersectR  = IntersectR3
	differenceR = DifferenceR3
	outset      = Outset3
	shell       = Shell3
	getBox      = getBox3
	getImplicit = getImplicit3
	implicit a b= EmbedBoxedObj3 (a,b)

union = unionR 0
difference = differenceR 0
intersect = intersectR 0

-- 3D operations

extrudeR = ExtrudeR

extrudeRM = ExtrudeRM

rotateExtrude = RotateExtrude

extrudeOnEdgeOf = ExtrudeOnEdgeOf

rotate3 = Rotate3


pack3 :: ℝ2 -> ℝ -> [SymbolicObj3] -> Maybe SymbolicObj3
pack3 (dx, dy) sep objs = 
	let
		boxDropZ (P (a,b,c),P (d,e,f)) = (P (a,b),P (d,e))
		withBoxes :: [(Box2, SymbolicObj3)]
		withBoxes = map (\obj -> ( boxDropZ $ getBox3 obj, obj)) objs
	in case pack (P (0,0),P (dy,dy)) sep withBoxes of
			(a, []) -> Just $ union $ map (\((x,y),obj) -> translate (x,y,0) obj) a
			_ -> Nothing
				

-- 2D operations

rotate = Rotate2


pack2 :: ℝ2 -> ℝ -> [SymbolicObj2] -> Maybe SymbolicObj2
pack2 (dx, dy) sep objs = 
	let
		withBoxes :: [(Box2, SymbolicObj2)]
		withBoxes = map (\obj -> ( getBox2 obj, obj)) objs
	in case pack (P (0,0),P (dy,dy)) sep withBoxes of
			(a, []) -> Just $ union $ map (\((x,y),obj) -> translate (x,y) obj) a
			_ -> Nothing

