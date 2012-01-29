-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Operations.Definitions where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators




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

	rotateXY ::
		ℝ       -- ^ Amount to rotate by
		-> obj  -- ^ Object to rotate
		-> obj  -- ^ Resulting rotated object		
	
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





-- | Operations that involve an idea of how far you are outwards
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


-- | Operations that are specific to some part of a
--   2D-3D dimensional object pair.
class PairObj obj2 vec2 obj3 vec3 
              | obj2 -> vec2, obj3 -> vec3, obj2 -> obj3, obj3 -> obj2 
      where

	-- | Extrude a rounded object
	extrudeR :: 
		ℝ       -- ^ Radius of rounding
		-> obj2 -- ^ 2D Object to extrude
		-> ℝ    -- ^ length to extrude it
		-> obj3 -- ^ Resulting 3D object

	-- | Extrude a rounded 2D object, modifying it over height
	--   Comment: Technically, extrudeR = extrudeRMod id, but then we 
	--    couldn't be clever with symbolics :) 
	extrudeRMod :: 
		ℝ       -- ^ Radius of rounding
		-> (ℝ -> ℝ2 -> ℝ2) -- ^ Function to modify each layer:
		                   -- Height transforming at,
		                   -- Input 2D transform,
		                   -- New point!
		-> obj2      -- ^ 2D Object to extrude
		-> ℝ         -- ^ length to extrude it
		-> obj3      -- ^ Resulting 3D object

	-- | Extrude one 2D object about the edge of another.
	--   Example: 2 circles produce a torus
	--   Comment: extrudeOnEdgeOf a b can be thought of as a 
	--   projection of a×b.
	extrudeOnEdgeOf ::
		obj2     -- ^ Object to extrude
		-> obj2  -- ^ Object to extrude along the edge of
		-> obj3  -- ^ Resulting 3D object

	-- | Like openscad rotate...
	--   Comment: Rotations are not abelian -- watch out!
	rotate3 ::
		(ℝ, ℝ, ℝ)   -- ^ Rotater (YZ, XZ, XY)
		-> obj3     -- ^ Object to rotate
		-> obj3     -- ^ Resulting object

