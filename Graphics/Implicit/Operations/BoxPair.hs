-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Operations.BoxPair where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations.Definitions


-- | Operations that are specific to some part of a
--   2D-3D dimensional object pair.
instance PairObj Box2 ℝ2 Box3 ℝ3 where

	extrudeR _ ((x1,y1),(x2,y2)) h = ((x1,y1,0),(x2,y2,h))
	-- This is kind of tricky, because we have no idea what kind of crazy function will come up with...
	-- So we're going to give them 3x play space. We'll mention people should mostly shrink objs...
	-- I fully intend to do crazy symbolic stuff almost everywhere, so I'm not too worried about speed.
	extrudeRMod _ _ ((x1,y1),(x2,y2)) h = ((x1 - dx, y1 - dy, 0),(x2 + dx, y2+ dy, h)) 
		where
			dx = x2 - x1
			dy = y2 - y1
	extrudeOnEdgeOf ((ax1,ay1),(ax2,ay2)) ((bx1,by1),(bx2,by2)) = 
		((bx1+ax1, by1+ax1, ay2), (bx2+ax2, by2+ax2, ay2))
	rotate3 _ ((x1,y1, z1),(x2,y2, z2)) = ( (-d, -d, -d), (d, d, d) )
		where
			d = (sqrt 2 *) $ maximum $ map abs [x1, x2, y1, y2, z1, z2]

