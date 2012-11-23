-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh) where

import Graphics.Implicit.Definitions
import Control.Parallel (par, pseq)
import Data.AffineSpace.Point

-- | getContour gets a polyline describe the edge of your 2D
--  object. It's really the only function in this file you need
--  to care about from an external perspective.

getContourMesh :: 𝔼2 -> 𝔼2 -> ℝ2 -> Obj2 -> [(𝔼2,𝔼2,𝔼2)]
getContourMesh (P (x1, y1)) (P (x2, y2)) (dx, dy) obj = 
	let
		-- How many steps will we take on each axis?
		nx = fromIntegral $ ceiling $ (x2 - x1) / dx
		ny = fromIntegral $ ceiling $ (y2 - y1) / dy
		-- Divide it up and compute the polylines
		trisOnGrid :: [[[(𝔼2,𝔼2,𝔼2)]]]
		trisOnGrid = [[getSquareTriangles
		           (P (x1 + (x2 - x1)*mx/nx,     y1 + (y2 - y1)*my/ny))
		           (P (x1 + (x2 - x1)*(mx+1)/nx, y1 + (y2 - y1)*(my+1)/ny))
		           obj
		     | mx <- [0.. nx-1] ] | my <- [0..ny-1] ]
		triangles = concat $ concat trisOnGrid
	in
		triangles
		

-- | This function gives line segments to divide negative interior
--  regions and positive exterior ones inside a square, based on its 
--  values at its vertices.
--  It is based on the linearly-interpolated marching squares algorithm.

getSquareTriangles :: 𝔼2 -> 𝔼2 -> Obj2 -> [(𝔼2,𝔼2,𝔼2)]
getSquareTriangles (P (x1, y1)) (P (x2, y2)) obj = 
	let 
		(x,y) = (x1, y1)

		-- Let's evlauate obj at a few points...
		x1y1 = obj $ P (x1, y1)
		x2y1 = obj $ P (x2, y1)
		x1y2 = obj $ P (x1, y2)
		x2y2 = obj $ P (x2, y2)
		c = obj $ P ((x1+x2)/2, (y1+y2)/2)

		dx = x2 - x1
		dy = y2 - y1

		-- linearly interpolated midpoints on the relevant axis
		--             midy2
		--      _________*__________
		--     |                    |
		--     |                    |
		--     |                    |
		--midx1*                    * midx2
		--     |                    |
		--     |                    |
		--     |                    |
		--     -----------*----------
		--              midy1

		midx1 = P (x,                       y + dy*x1y1/(x1y1-x1y2))
		midx2 = P (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
		midy1 = P (x + dx*x1y1/(x1y1-x2y1), y )
		midy2 = P (x + dx*x1y2/(x1y2-x2y2), y + dy)

		square a b c d = [(a,b,c), (a,c,d)]

	in case (x1y2 <= 0, x2y2 <= 0,
	         x1y1 <= 0, x2y1 <= 0) of
		-- Yes, there's some symetries that could reduce the amount of code...
		-- But I don't think they're worth exploiting...
		(True,  True, 
		 True,  True)  -> square (P (x1,y1)) (P (x2,y1)) (P (x2,y2)) (P (x1,y2))
		(False, False,
		 False, False) -> []
		(True,  True, 
		 False, False) -> square midx1 midx2 (P (x2,y2)) (P (x1,y2))
		(False, False,
		 True,  True)  -> square (P (x1,y1)) (P (x2,y1)) midx2 midx1 
		(False, True, 
		 False, True)  -> square midy1 (P (x2,y1)) (P (x2,y2)) midy2
		(True,  False,
		 True,  False) -> square (P (x1,y1)) midy1 midy2 (P (x1,y2))
		(True,  False,
		 False, False) -> [(P (x1,y2), midx1, midy2)]
		(False, True, 
		 True,  True)  -> 
			[(midx1, P (x1,y1), midy2), (P (x1,y1), P (x2,y1), midy2), (midy2, P (x2,y1), P (x2,y2))]
		(True,  True, 
		 False, True)  -> 
			[(P (x1,y2), midx1, P (x2,y2)), (midx1, midy1, P (x2,y2)), (P (x2,y2), midy1, P (x2,y1))] 
		(False, False,
		 True,  False) -> [(midx1, P (x1,y1), midy1)]
		(True,  True, 
		 True,  False) -> 
			[(midy1,midx2,P (x2,y2)), (P (x2,y2), P (x1,y2), midy1), (midy1, P (x1,y2), P (x1,y1))]
		(False, False,
		 False, True)  -> [(midx2, midy1, P (x2,y1))]
		(True,  False,
		 True,  True)  -> 
			[(midy2, P (x2,y1), midx2), (P (x2,y1), midy2, P (x1,y1)), (P (x1,y1), midy2, P (x1,y2))]
		(False, True, 
		 False, False) -> [(midx2, P (x2,y2), midy2)]
		(True,  False,
		 False, True)  -> if c > 0
			then [(P (x1,y2), midx1, midy2), (P (x2,y1), midy1, midx2)]
			else [] --[[midx1, midy1], [midx2, midy2]]
		(False, True, 
		 True,  False) -> if c <= 0
			then [] --[[midx1, midy2], [midx2, midy1]]
			else [(P (x1,y1), midy1, midx1), (P (x2,y2), midx2, midy2)] --[[midx1, midy1], [midx2, midy2]]



