-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.GetSegs where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.RefineSegs (refine)

getSegs' (x1, y1) (x2, y2) obj (midx1V,midx2V,midy1V,midy2V) = 
	let
		x1y1 = obj (x1, y1)
		x2y1 = obj (x2, y1)
		x1y2 = obj (x1, y2)
		x2y2 = obj (x2, y2)
	in
		getSegs (x1, y1) (x2, y2) obj (x1y1, x2y1, x1y2, x2y2) (midx1V,midx2V,midy1V,midy2V)

getSegs :: ℝ2 -> ℝ2 -> Obj2 -> (ℝ,ℝ,ℝ,ℝ) -> (ℝ,ℝ,ℝ,ℝ) -> [Polyline]
{-- INLINE getSegs #-}
getSegs (x1, y1) (x2, y2) obj (x1y1, x2y1, x1y2, x2y2) (midx1V,midx2V,midy1V,midy2V) = 
	let 
		(x,y) = (x1, y1)

		-- Let's evlauate obj at a few points...
		c = obj ((x1+x2)/2, (y1+y2)/2)

		dx = x2 - x1
		dy = y2 - y1
		res = sqrt (dx*dy)

		midx1 = (x,      midx1V )
		midx2 = (x + dx, midx2V )
		midy1 = (midy1V , y )
		midy2 = (midy2V, y + dy)

		notPointLine (p1:p2:[]) = p1 /= p2

	in map (refine res obj) . filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
	                                 x1y1 <= 0, x2y1 <= 0) of

		-- Yes, there's some symetries that could reduce the amount of code...
		-- But I don't think they're worth exploiting...
		(True,  True, 
		 True,  True)  -> []
		(False, False,
		 False, False) -> []
		(True,  True, 
		 False, False) -> [[midx1, midx2]]
		(False, False,
		 True,  True)  -> [[midx2, midx1]]
		(False, True, 
		 False, True)  -> [[midy2, midy1]]
		(True,  False,
		 True,  False) -> [[midy1, midy2]]
		(True,  False,
		 False, False) -> [[midx1, midy2]]
		(False, True, 
		 True,  True)  -> [[midy2, midx1]]
		(True,  True, 
		 False, True)  -> [[midx1, midy1]]
		(False, False,
		 True,  False) -> [[midy1, midx1]]
		(True,  True, 
		 True,  False) -> [[midy1, midx2]]
		(False, False,
		 False, True)  -> [[midx2, midy1]]
		(True,  False,
		 True,  True)  -> [[midx2, midy2]]
		(False, True, 
		 False, False) -> [[midy2, midx2]]
		(True,  False,
		 False, True)  -> if c <= 0
			then [[midx1, midy1], [midx2, midy2]]
			else [[midx1, midy2], [midx2, midy1]]
		(False, True, 
		 True,  False) -> if c <= 0
			then [[midy2, midx1], [midy1, midx2]]
			else [[midy1, midx1], [midy2, midx2]]

