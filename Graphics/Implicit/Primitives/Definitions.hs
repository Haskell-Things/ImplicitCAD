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
	rect3R ::
		ℝ        -- ^ Rounding of corners
		-> ℝ3    -- ^ Bottom// corner
		-> ℝ3    -- ^ Top right... corner
		-> obj   -- ^ Resuting cube - (0,0,0) is bottom left...
	cylinder2 ::
		ℝ         -- ^ Radius of the cylinder	
		-> ℝ      -- ^ Second radius of the cylinder
		-> ℝ      -- ^ Height of the cylinder
		-> obj    -- ^ Resulting cylinder


class PrimitiveSupporter2 obj where

	circle ::
		ℝ        -- ^ radius of the circle
		-> obj   -- ^ resulting circle
	rectR ::
		ℝ
		-> ℝ2     -- ^ Bottom left corner
		-> ℝ2     -- ^ Top right corner
		-> obj    -- ^ Resulting square (bottom right = (0,0) )
	polygonR ::
		ℝ         -- ^ Rouding of the polygon
		-> [ℝ2]   -- ^ Verticies of the polygon
		 -> obj   -- ^ Resulting polygon




