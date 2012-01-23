-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Primitives.Definitions where

import Graphics.Implicit.Definitions


-- Basic Primitive 3D Objects; We can make the others from here.
class PrimitiveSupporter3 obj where
	sphere ::
		ℝ         -- ^ Radius of the sphere
		-> obj    -- ^ Resulting sphere
	cubeV ::
		ℝ3        -- ^ Dimensions of the cube
		 -> obj   -- ^ Resuting cube - (0,0,0) is bottom left...
	cylinder2 ::
		ℝ         -- ^ Radius of the cylinder	
		-> ℝ      -- ^ Second radius of the cylinder
		-> ℝ      -- ^ Height of the cylinder
		-> obj    -- ^ Resulting cylinder
	torus ::
		ℝ         -- ^ radius of the rotated circle of a torus
		-> ℝ      -- ^ radius of the circle rotationaly extruded on of a torus
		-> obj    -- ^ resulting torus


class PrimitiveSupporter2 obj where

	circle ::
		ℝ        -- ^ radius of the circle
		-> obj   -- ^ resulting circle
	squareV ::
		ℝ2        -- ^ (x width, y width)
		-> obj    -- ^ Resulting square (bottom right = (0,0) )
	polygon ::
		[ℝ2]      -- ^ Verticies of the polygon
		 -> obj   -- ^ Resulting polygon




