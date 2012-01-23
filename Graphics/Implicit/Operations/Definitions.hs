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







