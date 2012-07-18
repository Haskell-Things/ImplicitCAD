-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingCubes (getMesh, getMesh') where

import Graphics.Implicit.Definitions
import Control.Parallel.Strategies (using, rdeepseq, parListChunk)

getMesh = getMesh'

getMesh' :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh' (x1, y1, z1) (x2, y2, z2) res obj = 
	let
		dx = x2-x1
		dy = y2-y1
		dz = z2-z1
		-- How many steps will we take on each axis?
		nx = fromIntegral $ ceiling $ dx / res
		ny = fromIntegral $ ceiling $ dy / res
		nz = fromIntegral $ ceiling $ dz / res
		vals = [[[ obj (x1 + dx*mx/nx, y1 + dy*my/ny, z1 + dz*mz/nz) 
		       | mz <- [0..nz] ] | my <- [0..ny] ] | mx <- [0..nx] ] 
		       `using` (parListChunk 2 rdeepseq)
		tris = [ let (x,y,z) = (floor mx, floor my, floor mz) in 
		           getCubeTriangles 
		           (x1+dx*mx/nx,y1+dy*my/ny,z1+dz*mz/nz) 
		           (x1+dx*(mx+1)/nx,y1+dy*(my+1)/ny,z1+dz*(mz+1)/nz)
		           (vals !! (x  ) !! (y  ) !! (z  )) (vals !! (x+1) !! (y  ) !! (z  ))
		           (vals !! (x  ) !! (y+1) !! (z  )) (vals !! (x+1) !! (y+1) !! (z  ))
		           (vals !! (x  ) !! (y  ) !! (z+1)) (vals !! (x+1) !! (y  ) !! (z+1))
		           (vals !! (x  ) !! (y+1) !! (z+1)) (vals !! (x+1) !! (y+1) !! (z+1))
		       | mx <- [0..nx-1], my <- [0..ny-1], mz <- [0..nz-1] 
		       ] `using` (parListChunk (floor nx* floor ny*(max 1 $ div (floor nz) 32)) rdeepseq)
	in concat tris

{-getMesh' :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh' (x1, y1, z1) (x2, y2, z2) res obj = 
	let
		dx = x2-x1
		dy = y2-y1
		dz = z2-z1
		-- How many steps will we take on each axis?
		nx = fromIntegral $ ceiling $ dx / res
		ny = fromIntegral $ ceiling $ dy / res
		nz = fromIntegral $ ceiling $ dz / res
		-- Divide it up and compute the polylines
		triangles :: [TriangleMesh]
		triangles = [getCubeTriangles
		           (x1 + dx*mx/nx,     y1 + dy*my/ny,     z1 + dz*mz/nz)
		           (x1 + dx*(mx+1)/nx, y1 + dy*(my+1)/ny, z1 + dz*(mz+1)/nz)
		           obj
		     | mx <- [0.. nx-1], my <- [0..ny-1], mz <- [0..nz-1] ]
		       `using` (parListChunk (div (floor nz) 16) rdeepseq)
	in
		concat triangles
-}

getCubeTriangles :: ℝ3 -> ℝ3 -> ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> [Triangle]
{-# INLINE getCubeTriangles #-}
getCubeTriangles (x1, y1, z1) (x2, y2, z2) x1y1z1 x2y1z1 x1y2z1 x2y2z1 x1y1z2 x2y1z2 x1y2z2 x2y2z2 =
	let
		
		(x,y,z) = (x1, y1, z1)
		dx = x2 - x1
		dy = y2 - y1
		dz = z2 - z1
		
		--{- Linearly interpolated
		x1y1 = (x,    y,    z+dz*x1y1z1/(x1y1z1-x1y1z2))
		x1y2 = (x,    y+dy, z+dz*x1y2z1/(x1y2z1-x1y2z2))
		x2y1 = (x+dx, y,    z+dz*x2y1z1/(x2y1z1-x2y1z2))
		x2y2 = (x+dx, y+dy, z+dz*x2y2z1/(x2y2z1-x2y2z2))

		x1z1 = (x,    y+dy*x1y1z1/(x1y1z1-x1y2z1), z)
		x1z2 = (x,    y+dy*x1y1z2/(x1y1z2-x1y2z2), z+dz)
		x2z1 = (x+dx, y+dy*x2y1z1/(x2y1z1-x2y2z1), z)
		x2z2 = (x+dx, y+dy*x2y1z2/(x2y1z2-x2y2z2), z+dz)

		y1z1 = (x+dx*x1y1z1/(x1y1z1-x2y1z1), y,    z)
		y1z2 = (x+dx*x1y1z2/(x1y1z2-x2y1z2), y,    z+dz)
		y2z1 = (x+dx*x1y2z1/(x1y2z1-x2y2z1), y+dy, z)
		y2z2 = (x+dx*x1y2z2/(x1y2z2-x2y2z2), y+dy, z+dz)

		--}
		{- Non-linearly interpolated

		x1y1 = (x,    y,    z+dz/2)
		x1y2 = (x,    y+dy, z+dz/2)
		x2y1 = (x+dx, y,    z+dz/2)
		x2y2 = (x+dx, y+dy, z+dz/2)

		x1z1 = (x,    y+dy/2, z)
		x1z2 = (x,    y+dy/2, z+dz)
		x2z1 = (x+dx, y+dy/2, z)
		x2z2 = (x+dx, y+dy/2, z+dz)

		y1z1 = (x+dx/2, y,   z)
		y1z2 = (x+dx/2, y,   z+dz)
		y2z1 = (x+dx/2, y+dy,z)
		y2z2 = (x+dx/2, y+dy,z+dz)
		--}

		-- Convenience function
		square a b c d = [(a,b,c),(d,a,c)]
		rsquare a b c d = [(c,b,a),(c,a,d)]
		rev (a, b, c) = (c, b, a)
	in case 
		-- whether the vertices are "in" or "out" form the topological 
		-- basis of our triangles constructions. We must consider every 
		-- possible case.

		-- We arrange the vertices in a human readable way

		-- BOTTOM LAYER             TOP LAYER
		(x1y2z1<=0, x2y2z1<=0,    x1y2z2<=0, x2y2z2<=0,
		 x1y1z1<=0, x2y1z1<=0,    x1y1z2<=0, x2y1z2<=0)
	of

		-- There are 256 cases to implement.
		-- Only about half are, but they're the most common ones.
		-- In practice, this has no issues redering reasonable objects.

		-- Yes, there's some symetries that could reduce the amount of code...
		-- But I don't think they're worth exploiting...
		-- In particular, since we're not implementing any case, 
		-- it would make catching the ones we don't implement... problematic.

		-- Uniform cases = empty
		(False,False,    False,False,
		 False,False,    False,False) -> []

		(True, True,     True, True,
		 True, True,     True, True ) -> []

		-- 2 uniform layers

		(True, True,     False,False,
		 True, True,     False,False) -> square x1y1 x2y1 x2y2 x1y2

		(False,False,    True, True,
		 False,False,    True, True ) -> rsquare x1y1 x2y1 x2y2 x1y2

		(True, True,     True, True,
		 False,False,    False,False) -> square x1z1 x2z1 x2z2 x1z2

		(False,False,    False,False,
		 True, True,     True, True ) -> rsquare x1z1 x2z1 x2z2 x1z2

		(False,True,     False,True,
		 False,True,     False,True ) -> rsquare y1z1 y2z1 y2z2 y1z2

		(True, False,    True, False,
		 True, False,    True, False) -> square y1z1 y2z1 y2z2 y1z2

		-- single z column

		(True, False,    True, False,
		 False,False,    False,False) -> square x1z1 y2z1 y2z2 x1z2

		(False,True,     False,True,
		 False,False,    False,False) -> rsquare x2z1 y2z1 y2z2 x2z2

		(False,False,    False,False,
		 True, False,    True, False) -> rsquare x1z1 y1z1 y1z2 x1z2

		(False,False,    False,False,
		 False,True,     False,True ) -> rsquare y1z1 x2z1 x2z2 y1z2

		(False,True,     False,True,
		 True, True,     True, True ) -> rsquare x1z1 y2z1 y2z2 x1z2

		(True, False,    True, False,
		 True, True,     True, True ) -> square x2z1 y2z1 y2z2 x2z2

		(True, True,     True, True, 
		 False,True,     False,True ) -> square x1z1 y1z1 y1z2 x1z2

		(True, True,     True, True, 
		 True, False,    True, False) -> square y1z1 x2z1 x2z2 y1z2

		-- single y column

		(True, False,    False,False,
		 True, False,    False,False) -> square x1y1 y1z1 y2z1 x1y2

		(False,True,     False,False,
		 False,True,     False,False) -> rsquare x2y1 y1z1 y2z1 x2y2

		(False,False,    True, False,
		 False,False,    True, False) -> rsquare x1y1 y1z2 y2z2 x1y2

		(False,False,    False,True, 
		 False,False,    False,True ) -> square x2y1 y1z2 y2z2 x2y2

		(False,True,     True, True,
		 False,True,     True, True) -> rsquare x1y1 y1z1 y2z1 x1y2

		(True, False,    True, True,
		 True, False,    True, True) -> square x2y1 y1z1 y2z1 x2y2

		(True, True,     False, True,
		 True, True,     False, True) -> square x1y1 y1z2 y2z2 x1y2

		(True, True,     True, False,
		 True, True,     True, False) -> rsquare x2y1 y1z2 y2z2 x2y2

		-- single x column

		(True, True,     False,False,
		 False,False,    False,False) -> square x1y2 x1z1 x2z1 x2y2

		(False,False,    False,False,
		 True, True,     False,False) -> rsquare x1y1 x1z1 x2z1 x2y1

		(False,False,    True, True,
		 False,False,    False,False) -> rsquare x1y2 x1z2 x2z2 x2y2

		(False,False,    False,False,
		 False,False,    True, True ) -> square x1y1 x1z2 x2z2 x2y1

		(False,False,    True, True,
		 True, True,     True, True ) -> rsquare x1y2 x1z1 x2z1 x2y2

		(True, True,     True, True, 
		 False,False,    True, True ) -> square x1y1 x1z1 x2z1 x2y1

		(True, True,     False,False,
		 True, True,     True, True ) -> square x1y2 x1z2 x2z2 x2y2

		(True, True,     True, True,
		 True, True,     False,False) -> rsquare x1y1 x1z2 x2z2 x2y1

		-- lone points

		(True, False,    False,False,
		 False,False,    False,False) -> [(x1z1, y2z1, x1y2)]

		(False,True,     False,False,
		 False,False,    False,False) -> [rev (x2z1, y2z1, x2y2)]

		(False,False,    False,False,
		 True, False,    False,False) -> [rev (x1z1, y1z1, x1y1)]

		(False,False,    False,False,
		 False,True,     False,False) -> [(x2z1, y1z1, x2y1)]

		(False,False,    True, False,
		 False,False,    False,False) -> [rev (x1z2, y2z2, x1y2)]

		(False,False,    False,True,
		 False,False,    False,False) -> [(x2z2, y2z2, x2y2)]

		(False,False,    False,False,
		 False,False,    True, False) -> [(x1z2, y1z2, x1y1)]

		(False,False,    False,False,
		 False,False,    False,True ) -> [rev (x2z2, y1z2, x2y1)]

		(False,True,     True, True,
		 True, True,     True, True ) -> [rev (x1z1, y2z1, x1y2)]

		(True, False,    True, True, 
		 True, True,     True, True ) -> [(x2z1, y2z1, x2y2)]

		(True, True,     True, True, 
		 False,True,     True, True ) -> [(x1z1, y1z1, x1y1)]

		(True, True,     True, True,
		 True, False,    True, True ) -> [rev (x2z1, y1z1, x2y1)]

		(True, True,     False,True, 
		 True, True,     True, True ) -> [(x1z2, y2z2, x1y2)]

		(True, True,     True, False,
		 True, True,     True, True ) -> [rev (x2z2, y2z2, x2y2)]

		(True, True,     True, True,
		 True, True,     False,True ) -> [rev (x1z2, y1z2, x1y1)]

		(True, True,     True, True, 
		 True, True,     True, False) -> [(x2z2, y1z2, x2y1)]

		-- z flat + 1

		(False,False,    True, False,
		 False,False,    True, True) -> [rev (x1y1,x2y1,x2z2), rev (x1y1,x2z2,y2z2), rev (x1y1,y2z2,x1y2)]

		(True, True,    False,True,
		 True, True,    False,False) -> [(x1y1,x2y1,x2z2), (x1y1,x2z2,y2z2), (x1y1,y2z2,x1y2)]

		(False,False,    False,True,
		 False,False,    True, True) -> [(x2y1,x1y1,x1z2), (x2y1,x1z2,y2z2), (x2y1,y2z2,x2y2)]

		(True, True,    True, False,
		 True, True,    False,False) -> [rev (x2y1,x1y1,x1z2), rev (x2y1,x1z2,y2z2), rev (x2y1,y2z2,x2y2)]

		(False,False,    True, True,
		 False,False,    True, False) -> [(x1y2,x2y2,x2z2), (x1y2,x2z2,y1z2), (x1y2,y1z2,x1y1)]

		(True, True,    False,False,
		 True, True,    False,True ) -> [rev (x1y2,x2y2,x2z2), rev (x1y2,x2z2,y1z2), rev (x1y2,y1z2,x1y1)]

		(False,False,    True, True,
		 False,False,    False,True) -> [rev (x2y2,x1y2,x1z2), rev (x2y2,x1z2,y1z2), rev (x2y2,y1z2,x2y1)]

		(True, True,    False,False,
		 True, True,    True, False) -> [(x2y2,x1y2,x1z2), (x2y2,x1z2,y1z2), (x2y2,y1z2,x2y1)]



		(True, False,    False,False,
		 True, True,     False,False) -> [(x1y1,x2y1,x2z1), (x1y1,x2z1,y2z1), (x1y1,y2z1,x1y2)]

		(False,True,     True, True,
		 False,False,     True, True) -> [rev (x1y1,x2y1,x2z1), rev (x1y1,x2z1,y2z1), rev (x1y1,y2z1,x1y2)]

		(False,True,    False,False,
		 True, True,    False,False) -> [rev (x2y1,x1y1,x1z1), rev (x2y1,x1z1,y2z1), rev (x2y1,y2z1,x2y2)]

		(True, False,     True, True,
		 False,False,     True, True) -> [(x2y1,x1y1,x1z1), (x2y1,x1z1,y2z1), (x2y1,y2z1,x2y2)]

		(True, True,    False,False,
		 True, False,    False,False) -> [rev (x1y2,x2y2,x2z1), rev (x1y2,x2z1,y1z1), rev (x1y2,y1z1,x1y1)]

		(False,False,     True, True,
		 False,True,     True, True) -> [(x1y2,x2y2,x2z1), (x1y2,x2z1,y1z1), (x1y2,y1z1,x1y1)]

		(True, True,    False,False,
		 False,True,    False,False) -> [(x2y2,x1y2,x1z1), (x2y2,x1z1,y1z1), (x2y2,y1z1,x2y1)]

		(False,False,     True, True,
		 True, False,     True, True) -> [rev (x2y2,x1y2,x1z1), rev (x2y2,x1z1,y1z1), rev (x2y2,y1z1,x2y1)]



		-- y flat + 1

		(True, False,    True, True,
		 True, False,    True, False) -> [(y2z1,x2y2,x2z2),(y2z1,x2z2,y1z1),(y1z1,x2z2,y1z2)]

		(False,True,     False,False,
		 False,True,     False,True ) -> [rev (y2z1,x2y2,x2z2),rev (y2z1,x2z2,y1z1),rev (y1z1,x2z2,y1z2)]

		(True, False,    True, False,
		 True, False,    True, True ) -> [rev (y1z1,x2y1,x2z2),rev (y1z1,x2z2,y2z1),rev (y2z1,x2z2,y2z2)]

		(False,True,     False,True,
		 False,True,     False,False) -> [(y1z1,x2y1,x2z2),(y1z1,x2z2,y2z1),(y2z1,x2z2,y2z2)]

		(False,True,     True, True,
		 False,True,     False,True ) -> [rev (y2z1,x1y2,x1z2),rev (y2z1,x1z2,y1z1),rev (y1z1,x1z2,y1z2)]

		(True, False,    False,False,
		 True, False,    True, False) -> [(y2z1,x1y2,x1z2),(y2z1,x1z2,y1z1),(y1z1,x1z2,y1z2)]

		(False,True,     False,True,
		 False,True,     True, True ) -> [(y1z1,x1y1,x1z2),(y1z1,x1z2,y2z1),(y2z1,x1z2,y2z2)]

		(True, False,    True, False,
		 True, False,    False,False) -> [rev (y1z1,x1y1,x1z2),rev (y1z1,x1z2,y2z1),rev (y2z1,x1z2,y2z2)]



		(True, True,    True, False,
		 True, False,    True, False) -> [rev (y2z2,x2y2,x2z1),rev (y2z2,x2z1,y1z2),rev (y1z2,x2z1,y1z1)]

		(False,False,    False,True,
		 False,True,     False,True ) -> [(y2z2,x2y2,x2z1),(y2z2,x2z1,y1z2),(y1z2,x2z1,y1z1)]

		(True, False,    True, False,
		 True, True,     True, False) -> [(y1z2,x2y1,x2z1),(y1z2,x2z1,y2z2),(y2z2,x2z1,y2z1)]

		(False,True,     False,True,
		 False,False,    False,True) -> [rev (y1z2,x2y1,x2z1),rev (y1z2,x2z1,y2z2),rev (y2z2,x2z1,y2z1)]

		(True, True,     False,True,
		 False,True,     False,True) -> [(y2z2,x1y2,x1z1),(y2z2,x1z1,y1z2),(y1z2,x1z1,y1z1)]

		(False,False,    True, False,
		 True, False,    True, False) -> [rev (y2z2,x1y2,x1z1),rev (y2z2,x1z1,y1z2),rev (y1z2,x1z1,y1z1)]

		(False,True,     False,True,
		 True, True,     False,True) -> [rev (y1z2,x1y1,x1z1),rev (y1z2,x1z1,y2z2),rev (y2z2,x1z1,y2z1)]

		(True, False,    True, False,
		 False,False,    True, False) -> [(y1z2,x1y1,x1z1),(y1z2,x1z1,y2z2),(y2z2,x1z1,y2z1)]



		-- x flat +1

		(True, True,     True, True,
		 False,False,    True, False) -> [(x1z1,x2z1,x1y1),(x1y1,x2z1,x2z2),(x1y1,x2z2,y1z2)]

		(False,False,    False,False,
		 True, True,     False,True ) -> [rev (x1z1,x2z1,x1y1),rev (x1y1,x2z1,x2z2),rev (x1y1,x2z2,y1z2)]

		(False,False,    True, False,
		 True, True,     True, True) -> [rev (x1z1,x2z1,x1y2),rev (x1y2,x2z1,x2z2),rev (x1y2,x2z2,y2z2)]

		(True, True,     False,True,
		 False,False,    False,False) -> [(x1z1,x2z1,x1y2),(x1y2,x2z1,x2z2),(x1y2,x2z2,y2z2)]

		(True, True,     True, True,
		 False,False,    False,True) -> [rev (x2z1,x1z1,x2y1),rev (x2y1,x1z1,x1z2),rev (x2y1,x1z2,y1z2)]

		(False,False,    False,False,
		 True, True,     True, False) -> [(x2z1,x1z1,x2y1),(x2y1,x1z1,x1z2),(x2y1,x1z2,y1z2)]

		(False,False,    False,True,
		 True, True,     True, True) -> [(x2z1,x1z1,x2y2),(x2y2,x1z1,x1z2),(x2y2,x1z2,y2z2)]

		(True, True,     True, False,
		 False,False,    False,False) -> [rev (x2z1,x1z1,x2y2),rev (x2y2,x1z1,x1z2),rev (x2y2,x1z2,y2z2)]


		(True, True,     True, True,
		 True, False,    False,False) -> [rev (x1z2,x2z2,x1y1),rev (x1y1,x2z2,x2z1),rev (x1y1,x2z1,y1z1)]

		(False,False,    False,False,
		 False,True,     True, True ) -> [(x1z2,x2z2,x1y1),(x1y1,x2z2,x2z1),(x1y1,x2z1,y1z1)]

		(True, False,    False,False,
		 True, True,     True, True ) -> [(x1z2,x2z2,x1y2),(x1y2,x2z2,x2z1),(x1y2,x2z1,y2z1)]

		(False,True,     True, True,
		 False,False,    False,False) -> [rev (x1z2,x2z2,x1y2),rev (x1y2,x2z2,x2z1),rev (x1y2,x2z1,y2z1)]

		(True, True,     True, True,
		 False,True,     False,False) -> [(x2z2,x1z2,x2y1),(x2y1,x1z2,x1z1),(x2y1,x1z1,y1z1)]

		(False,False,    False,False,
		 True, False,    True, True) -> [rev (x2z2,x1z2,x2y1),rev (x2y1,x1z2,x1z1),rev (x2y1,x1z1,y1z1)]

		(False,True,     False,False,
		 True, True,     True, True) -> [rev (x2z2,x1z2,x2y2),rev (x2y2,x1z2,x1z1),rev (x2y2,x1z1,y2z1)]

		(True, False,    True, True,
		 False,False,    False,False) -> [(x2z2,x1z2,x2y2),(x2y2,x1z2,x1z1),(x2y2,x1z1,y2z1)]



		(True, True,     True, False,
		 True, False,    False,False) -> [rev (x1y1,x1z2,y1z1),rev (y1z1,x1z2,y2z2),rev (y1z1,y2z2,x2z1),rev (x2z1,y2z2,x2y2)]

		(False,False,    False,True,
		 False,True,     True, True ) -> [(x1y1,x1z2,y1z1),(y1z1,x1z2,y2z2),(y1z1,y2z2,x2z1),(x2z1,y2z2,x2y2)]

		(True, True,     False,True,
		 False,True,     False,False) -> [(x2y1,x2z2,y1z1),(y1z1,x2z2,y2z2),(y1z1,y2z2,x1z1),(x1z1,y2z2,x1y2)]

		(False,False,    True, False,
		 True, False,    True, True ) -> [rev (x2y1,x2z2,y1z1),rev (y1z1,x2z2,y2z2),rev (y1z1,y2z2,x1z1),rev (x1z1,y2z2,x1y2)]




		(True, False,    False,False,
		 True, True,     True, False) -> [(x1y2,x1z2,y2z1),(y2z1,x1z2,y1z2),(y2z1,y1z2,x2z1),(x2z1,y1z2,x2y1)]

		(False,True,     True, True,
		 False,False,    False,True ) -> [rev (x1y2,x1z2,y2z1),rev (y2z1,x1z2,y1z2),rev (y2z1,y1z2,x2z1),rev (x2z1,y1z2,x2y1)]

		(False,True,     False,False,
		 True, True,     False,True ) -> [rev (x2y2,x2z2,y2z1),rev (y2z1,x2z2,y1z2),rev (y2z1,y1z2,x1z1),rev (x1z1,y1z2,x1y1)]

		(True, False,    True, True,
		 False,False,    True, False) -> [(x2y2,x2z2,y2z1),(y2z1,x2z2,y1z2),(y2z1,y1z2,x1z1),(x1z1,y1z2,x1y1)]


		-- O@ OO
		-- @O @O
		(False,True,     False, False,
		 True, False,    True,  False) -> square y1z1 x2z1 x2y2 y1z2
		                               ++ rsquare x1z1 y2z1 x2y2 x1z2
		                               ++ [(y1z2, x2y2, x1z2)]
		(False,True,     False, True,
		 True, False,    False, False) -> square y2z1 x1z1 x1y1 y2z2
		                               ++ rsquare x2z1 y1z1 x1y1 x2z2
		                               ++ [(y2z2, x1y1, x2z2)]
		(True, False,    True, False,
		 False,True,     False,False) -> rsquare y2z1 x2z1 x2y1 y2z2
		                               ++ square x1z1 y1z1 x2y1 x1z2
		                               ++ [(y2z2, x1z2, x2y1)]
		(True,False,     False,False,
		 False,True,     False,True) -> rsquare y1z1 x1z1 x1y2 y1z2
		                               ++ square x2z1 y2z1 x1y2 x2z2
		                               ++ [(y1z2, x2z2, x1y2)]
		-- OO O@  normals verified
		-- @O @O
		(False,False,    False, True,
		 True, False,    True,  False) -> rsquare y1z2 x2z2 x2y2 y1z1
		                               ++ square x1z2 y2z2 x2y2 x1z1
		                               ++ [(y1z1, x1z1, x2y2)]
		(False,True,     False, True,
		 False,False,    True,  False) -> rsquare y2z2 x1z2 x1y1 y2z1
		                               ++ square x2z2 y1z2 x1y1 x2z1
		                               ++ [(y2z1, x2z1, x1y1)]
		(True, False,    True, False,
		 False,False,    False,True) -> square y2z2 x2z2 x2y1 y2z1
		                               ++ rsquare x1z2 y1z2 x2y1 x1z1
		                               ++ [(y2z1, x2y1, x1z1)]
		(False,False,    True, False,
		 False,True,     False,True) -> square y1z2 x1z2 x1y2 y1z1
		                               ++ rsquare x2z2 y2z2 x1y2 x2z1
		                               ++ [(y1z1, x1y2, x2z1)]
		-- @O @@    -- normals verified
		-- O@ O@
		(True,False,     True, True,
		 False, True,    False,  True) -> rsquare y1z1 x2z1 x2y2 y1z2
		                               ++ square x1z1 y2z1 x2y2 x1z2
		                               ++ [(y1z2, x1z2, x2y2)]
		(True,False,     True, False,
		 False, True,    True, True) -> rsquare y2z1 x1z1 x1y1 y2z2
		                             ++ square x2z1 y1z1 x1y1 x2z2
		                             ++ [(y2z2, x2z2, x1y1)]
		(False, True,    False, True,
		 True,False,     True,True) -> square y2z1 x2z1 x2y1 y2z2
		                            ++ rsquare x1z1 y1z1 x2y1 x1z2
		                            ++ [(y2z2, x2y1, x1z2)]
		(False,True,     True,True,
		 True,False,     True,False) -> square y1z1 x1z1 x1y2 y1z2
		                             ++ rsquare x2z1 y2z1 x1y2 x2z2
		                             ++ [(y1z2, x1y2, x2z2)]
		-- @@ @O
		-- O@ O@
		(True,True,    True, False,
		 False, True,    False,  True) -> rsquare y1z2 x2z2 x2y2 y1z1
		                               ++ square x1z2 y2z2 x2y2 x1z1
		                               ++ [(y1z1, x1z1, x2y2)]
		(True,False,     True, False,
		 True,True,    False,  True) -> rsquare y2z2 x1z2 x1y1 y2z1
		                               ++ square x2z2 y1z2 x1y1 x2z1
		                               ++ [(y2z1, x2z1, x1y1)]
		(False, True,    False, True,
		 True,True,    True,False) -> square y2z2 x2z2 x2y1 y2z1
		                               ++ rsquare x1z2 y1z2 x2y1 x1z1
		                               ++ [(y2z1, x2y1, x1z1)]
		(True,True,    False, True,
		 True,False,     True,False) -> square y1z2 x1z2 x1y2 y1z1
		                               ++ rsquare x2z2 y2z2 x1y2 x2z1
		                               ++ [(y1z1, x1y2, x2z1)]


		-- @@ O@
		-- @O OO
		(True, True,     False, True,
		 True, False,    False, False) -> square x1y1 x1y2 y2z2 x2z2
		                               ++ square x1y1 x2z2 x2z1 y1z1
		(False,True,     False, True,
		 True, True,     False, False) -> square x1y1 x2y1 x2z2 y2z2
		                               ++ square x1y1 y2z2 y2z1 x1z1
		(False,True,     False, False,
		 True, True,     True,  False) -> square x2y2 x2y1 y1z2 x1z2
		                               ++ square x2y2 x1z2 x1z1 y2z1
		(True, True,     False, False,
		 True, False,    True,  False) -> square x2y2 x1y2 x1z2 y1z2
		                               ++ square x2y2 y1z2 y1z1 x2z1

		(True, False,    False, False,
		 True, True,     False, True ) -> square x1y2 x1y1 y1z2 x2z2
		                               ++ square x1y2 x2z2 x2z1 y2z1
		(True, True,     False, False,
		 False,True,     False, True ) -> square x1y2 x2y2 x2z2 y1z2
		                               ++ square x1y2 y2z2 y1z1 x1z1
		(True, True,     True,  False,
		 False,True,     False, False) -> square x2y1 x2y2 y2z2 x1z2
		                               ++ square x2y1 x1z2 x1z1 y1z1
		(True, False,    True, False,
		 True, True,     False,False)  -> square x2y1 x1y1 x1z2 y2z2
		                               ++ square x2y1 y1z2 y2z1 x2z1




--{-

		-- @O OO
		-- O@ OO

		(True, False,    False, False,
		 False,True,     False, False) -> square x1z1 y1z1 x1y2 x2y1 ++ square x2z1 y2z1 x1y2 x2y1

		(False,True,     False, False,
		 True, False,    False, False) -> square y1z1 x2z1 x2y2 x1y1 ++ square y2z1 x1z1 x2y2 x1y1

		(False, False,   True, False,
		 False, False,   False,True ) -> square x1z2 y1z2 x1y2 x2y1 ++ square x2z2 y2z2 x1y2 x2y1

		(False, False,   False,True,
		 False, False,   True, False) -> square y1z2 x2z2 x2y2 x1y1 ++ square y2z2 x1z2 x2y2 x1y1

		(False, True,    True, True,
		 True,False,     True, True) -> square x1z1 y1z1 x1y2 x2y1 ++ square x2z1 y2z1 x1y2 x2y1

		(True,False,     True, True,
		 False, True,    True, True) -> square y1z1 x2z1 x2y2 x1y1 ++ square y2z1 x1z1 x2y2 x1y1

		(True, True,   False, True,
		 True, True,   True,False ) -> square x1z2 y1z2 x1y2 x2y1 ++ square x2z2 y2z2 x1y2 x2y1

		(True, True,   True,False,
		 True, True,   False, True) -> square y1z2 x2z2 x2y2 x1y1 ++ square y2z2 x1z2 x2y2 x1y1
		---}



		-- Debuging or fault tolerance, tependin on how you comment it
		(a,b,  c,d,
		 e,f,  g,h) -> 
			{-let
				s b = if b then "#" else "O"
			in error $ "Unimplemnted Marching Cubes case:\n"
				++ s a ++ s b ++ " " ++ s c ++ s d ++ "\n"
				++ s e ++ s f  ++ " " ++ s g ++ s h ++ "\n"
			-- -} []

