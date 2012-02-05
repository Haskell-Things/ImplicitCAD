-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingCubes (getMesh, getMesh2) where

import Graphics.Implicit.Definitions
import Control.Parallel (par, pseq)

-- | getMesh gets a triangle mesh describe the boundary of your 3D
--  object. 
--  There are many getMesh functions in this file. THis one is the
--  simplest and should be least bug prone. Use it for debugging.
getMesh :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh (x1, y1, z1) (x2, y2, z2) res obj = 
	let
		-- How many steps will we take on each axis?
		nx = fromIntegral $ ceiling $ (x2 - x1) / res
		ny = fromIntegral $ ceiling $ (y2 - y1) / res
		nz = fromIntegral $ ceiling $ (y2 - y1) / res
		-- Divide it up and compute the polylines
		triangles :: [TriangleMesh]
		triangles = [getCubeTriangles
		           (x1 + (x2 - x1)*mx/nx,     y1 + (y2 - y1)*my/ny,     z1 + (z2 - z1)*mz/nz)
		           (x1 + (x2 - x1)*(mx+1)/nx, y1 + (y2 - y1)*(my+1)/ny, z1 + (z2 - z1)*(mz+1)/nz)
		           obj
		     | mx <- [0.. nx-1], my <- [0..ny-1], mz <- [0..nz-1] ]
	in
		concat $ triangles


getMesh2 (x1,y1,z1) (x2,y2,z2) res obj = 
	let 
		dx = abs $ x2 - x1
		dy = abs $ y2 - y1
		dz = abs $ z2 - z1
		d = maximum [dx, dy, dz]
		ffloor = fromIntegral . floor
		fceil = fromIntegral . ceiling
	in
		if (abs.obj) ( (x1 + x2)/2, (y1 + y2)/2, (z1 + z2)/2) > d*0.9 then []
		else
		if d <= res
		then getCubeTriangles (x1,y1,z1) (x1+res,y1+res,z1+res) obj
		else let
			xs = if dx <= res then [(x1, x2)] else [(x1,xm), (xm, x2)] 
				where xm = x1 + res * fceil ( ffloor (dx/res) / 2.0)
			ys = if dy <= res then [(y1, y2)] else [(y1,xm), (xm, y2)] 
				where xm = y1 + res * fceil ( ffloor (dy/res) / 2.0)
			zs = if dz <= res then [(z1, z2)] else [(z1,xm), (xm, z2)] 
				where xm = z1 + res * fceil ( ffloor (dz/res) / 2.0)
			partitions = [getMesh (x1', y1', z1') (x2', y2', z2') res obj
				|  (x1',x2') <- xs, (y1', y2') <- ys, (z1',z2') <- zs ]
		in
			concat partitions


getMesh3 (x1,y1,z1) (x2,y2,z2) res obj = 
	let 
		dx = abs $ x2 - x1
		dy = abs $ y2 - y1
		dz = abs $ z2 - z1
		d = maximum [dx, dy, dz]
		ffloor = fromIntegral . floor
		fceil = fromIntegral . ceiling
	in
		if (abs.obj) ( (x1 + x2)/2, (y1 + y2)/2, (z1 + z2)/2) > d*0.9 then []
		else
		if d <= res
		then getCubeTriangles (x1,y1,z1) (x1+res,y1+res,z1+res) obj
		else let
			xs = if dx <= res then [(x1, x2)] else [(x1,xm), (xm, x2)] 
				where xm = x1 + res * fceil ( ffloor (dx/res) / 2.0)
			ys = if dy <= res then [(y1, y2)] else [(y1,xm), (xm, y2)] 
				where xm = y1 + res * fceil ( ffloor (dy/res) / 2.0)
			zs = if dz <= res then [(z1, z2)] else [(z1,xm), (xm, z2)] 
				where xm = z1 + res * fceil ( ffloor (dz/res) / 2.0)
			partitions = [getMesh (x1', y1', z1') (x2', y2', z2') res obj
				|  (x1',x2') <- xs, (y1', y2') <- ys, (z1',z2') <- zs ]
		in
			foldr1 par partitions `pseq` concat partitions



-- | This monstrosity of a function gives triangles to divde negative interior
--  regions and positive exterior ones inside a cube, based on its vertices.
--  It is based on the linearly-interpolated marching cubes algorithm.

getCubeTriangles :: ℝ3 -> ℝ3 -> Obj3 -> [Triangle]
getCubeTriangles (x1, y1, z1) (x2, y2, z2) obj =
	let
		(x,y,z) = (x1, y1, z1)
		
		x1y1z1 = obj (x1, y1, z1)
		x2y1z1 = obj (x2, y1, z1)
		x1y2z1 = obj (x1, y2, z1)
		x2y2z1 = obj (x2, y2, z1)
		x1y1z2 = obj (x1, y1, z2)
		x2y1z2 = obj (x2, y1, z2)
		x1y2z2 = obj (x1, y2, z2)
		x2y2z2 = obj (x2, y2, z2)

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
		 False,False,    True, True ) -> square x1y1 x2y1 x2y2 x1y2

		(True, True,     True, True,
		 False,False,    False,False) -> square x1z1 x2z1 x2z2 x1z2

		(False,False,    False,False,
		 True, True,     True, True ) -> square x1z1 x2z1 x2z2 x1z2

		(False,True,     False,True,
		 False,True,     False,True ) -> square y1z1 y2z1 y2z2 y1z2

		(True, False,    True, False,
		 True, False,    True, False) -> square y1z1 y2z1 y2z2 y1z2


		-- z single column

		(True, False,    True, False,
		 False,False,    False,False) -> square x1z1 y2z1 y2z2 x1z2

		(False,True,     False,True,
		 False,False,    False,False) -> square x2z1 y2z1 y2z2 x2z2

		(False,False,    False,False,
		 True, False,    True, False) -> square x1z1 y1z1 y1z2 x1z2

		(False,False,    False,False,
		 False,True,     False,True ) -> square y1z1 x2z1 x2z2 y1z2

		(False,True,     False,True, 
		 True, True,     True, True ) -> square x1z1 y2z1 y2z2 x1z2

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
		 False,True,     False,False) -> square x2y1 y1z1 y2z1 x2y2

		(False,False,    True, False,
		 False,False,    True, False) -> square x1y1 y1z2 y2z2 x1y2

		(False,False,    False,True, 
		 False,False,    False,True ) -> square x2y1 y1z2 y2z2 x2y2

		(False,True,     True, True,
		 False,True,     True, True) -> square x1y1 y1z1 y2z1 x1y2

		(True, False,    True, True,
		 True, False,    True, True) -> square x2y1 y1z1 y2z1 x2y2

		(True, True,     False, True,
		 True, True,     False, True) -> square x1y1 y1z2 y2z2 x1y2

		(True, True,     True, False, 
		 True, True,     True, False) -> square x2y1 y1z2 y2z2 x2y2

		-- since x column

		(True, True,     False,False,
		 False,False,    False,False) -> square x1y2 x1z1 x2z1 x2y2

		(False,False,    False,False,
		 True, True,     False,False) -> square x1y1 x1z1 x2z1 x2y1

		(False,False,    True, True,
		 False,False,    False,False) -> square x1y2 x1z2 x2z2 x2y2

		(False,False,    False,False,
		 False,False,    True, True ) -> square x1y1 x1z2 x2z2 x2y1

		(False,False,    True, True, 
		 True, True,     True, True ) -> square x1y2 x1z1 x2z1 x2y2

		(True, True,     True, True, 
		 False,False,    True, True ) -> square x1y1 x1z1 x2z1 x2y1

		(True, True,     False,False,
		 True, True,     True, True ) -> square x1y2 x1z2 x2z2 x2y2

		(True, True,     True, True, 
		 True, True,     False,False) -> square x1y1 x1z2 x2z2 x2y1

		-- lone points

		(True, False,    False,False,
		 False,False,    False,False) -> [(x1z1, y2z1, x1y2)]

		(False,True,     False,False,
		 False,False,    False,False) -> [(x2z1, y2z1, x2y2)]

		(False,False,    False,False,
		 True, False,    False,False) -> [(x1z1, y1z1, x1y1)]

		(False,False,    False,False,
		 False,True,     False,False) -> [(x2z1, y1z1, x2y1)]

		(False,False,    True, False,
		 False,False,    False,False) -> [(x1z2, y2z2, x1y2)]

		(False,False,    False,True,
		 False,False,    False,False) -> [(x2z2, y2z2, x2y2)]

		(False,False,    False,False,
		 False,False,    True, False) -> [(x1z2, y1z2, x1y1)]

		(False,False,    False,False,
		 False,False,    False,True ) -> [(x2z2, y1z2, x2y1)]

		(False,True,     True, True, 
		 True, True,     True, True ) -> [(x1z1, y2z1, x1y2)]

		(True, False,    True, True, 
		 True, True,     True, True ) -> [(x2z1, y2z1, x2y2)]

		(True, True,     True, True, 
		 False,True,     True, True ) -> [(x1z1, y1z1, x1y1)]

		(True, True,     True, True, 
		 True, False,    True, True ) -> [(x2z1, y1z1, x2y1)]

		(True, True,     False,True, 
		 True, True,     True, True ) -> [(x1z2, y2z2, x1y2)]

		(True, True,     True, False,
		 True, True,     True, True ) -> [(x2z2, y2z2, x2y2)]

		(True, True,     True, True, 
		 True, True,     False,True ) -> [(x1z2, y1z2, x1y1)]

		(True, True,     True, True, 
		 True, True,     True, False) -> [(x2z2, y1z2, x2y1)]

		-- z flat + 1

		(False,False,    True, False,
		 False,False,    True, True) -> [(x1y1,x2y1,x2z2), (x1y1,x2z2,y2z2), (x1y1,y2z2,x1y2)]

		(True, True,    False,True,
		 True, True,    False,False) -> [(x1y1,x2y1,x2z2), (x1y1,x2z2,y2z2), (x1y1,y2z2,x1y2)]

		(False,False,    False,True,
		 False,False,    True, True) -> [(x2y1,x1y1,x1z2), (x2y1,x1z2,y2z2), (x2y1,y2z2,x2y2)]

		(True, True,    True, False,
		 True, True,    False,False) -> [(x2y1,x1y1,x1z2), (x2y1,x1z2,y2z2), (x2y1,y2z2,x2y2)]

		(False,False,    True, True,
		 False,False,    True, False) -> [(x1y2,x2y2,x2z2), (x1y2,x2z2,y1z2), (x1y2,y1z2,x1y1)]

		(True, True,    False,False,
		 True, True,    False,True ) -> [(x1y2,x2y2,x2z2), (x1y2,x2z2,y1z2), (x1y2,y1z2,x1y1)]

		(False,False,    True, True,
		 False,False,    False,True) -> [(x2y2,x1y2,x1z2), (x2y2,x1z2,y1z2), (x2y2,y1z2,x2y1)]

		(True, True,    False,False,
		 True, True,    True, False) -> [(x2y2,x1y2,x1z2), (x2y2,x1z2,y1z2), (x2y2,y1z2,x2y1)]



		(True, False,    False,False,
		 True, True,     False,False) -> [(x1y1,x2y1,x2z1), (x1y1,x2z1,y2z1), (x1y1,y2z1,x1y2)]

		(False,True,     True, True,
		 False,False,     True, True) -> [(x1y1,x2y1,x2z1), (x1y1,x2z1,y2z1), (x1y1,y2z1,x1y2)]

		(False,True,    False,False,
		 True, True,    False,False) -> [(x2y1,x1y1,x1z1), (x2y1,x1z1,y2z1), (x2y1,y2z1,x2y2)]

		(True, False,     True, True,
		 False,False,     True, True) -> [(x2y1,x1y1,x1z1), (x2y1,x1z1,y2z1), (x2y1,y2z1,x2y2)]

		(True, True,    False,False,
		 True, False,    False,False) -> [(x1y2,x2y2,x2z1), (x1y2,x2z1,y1z1), (x1y2,y1z1,x1y1)]

		(False,False,     True, True,
		 False,True,     True, True) -> [(x1y2,x2y2,x2z1), (x1y2,x2z1,y1z1), (x1y2,y1z1,x1y1)]

		(True, True,    False,False,
		 False,True,    False,False) -> [(x2y2,x1y2,x1z1), (x2y2,x1z1,y1z1), (x2y2,y1z1,x2y1)]

		(False,False,     True, True,
		 True, False,     True, True) -> [(x2y2,x1y2,x1z1), (x2y2,x1z1,y1z1), (x2y2,y1z1,x2y1)]

		-- y flat + 1

		(True, False,    True, True,
		 True, False,    True, False) -> [(y2z1,x2y2,x2z2),(y2z1,x2z2,y1z1),(y1z1,x2z2,y1z2)]

		(False,True,     False,False,
		 False,True,     False,True ) -> [(y2z1,x2y2,x2z2),(y2z1,x2z2,y1z1),(y1z1,x2z2,y1z2)]

		(True, False,    True, False,
		 True, False,    True, True ) -> [(y1z1,x2y1,x2z2),(y1z1,x2z2,y2z1),(y2z1,x2z2,y2z2)]

		(False,True,     False,True,
		 False,True,     False,False) -> [(y1z1,x2y1,x2z2),(y1z1,x2z2,y2z1),(y2z1,x2z2,y2z2)]

		(False,True,     True, True,
		 False,True,     False,True ) -> [(y2z1,x1y2,x1z2),(y2z1,x1z2,y1z1),(y1z1,x1z2,y1z2)]

		(True, False,    False,False,
		 True, False,    True, False) -> [(y2z1,x1y2,x1z2),(y2z1,x1z2,y1z1),(y1z1,x1z2,y1z2)]

		(False,True,     False,True,
		 False,True,     True, True ) -> [(y1z1,x1y1,x1z2),(y1z1,x1z2,y2z1),(y2z1,x1z2,y2z2)]

		(True, False,    True, False,
		 True, False,    False,False) -> [(y1z1,x1y1,x1z2),(y1z1,x1z2,y2z1),(y2z1,x1z2,y2z2)]



		(True, True,    True, False,
		 True, False,    True, False) -> [(y2z2,x2y2,x2z1),(y2z2,x2z1,y1z2),(y1z2,x2z1,y1z1)]

		(False,False,    False,True,
		 False,True,     False,True ) -> [(y2z2,x2y2,x2z1),(y2z2,x2z1,y1z2),(y1z2,x2z1,y1z1)]

		(True, False,    True, False,
		 True, True,     True, False) -> [(y1z2,x2y1,x2z1),(y1z2,x2z1,y2z2),(y2z2,x2z1,y2z1)]

		(False,True,     False,True,
		 False,False,    False,True) -> [(y1z2,x2y1,x2z1),(y1z2,x2z1,y2z2),(y2z2,x2z1,y2z1)]

		(True, True,     False,True,
		 False,True,     False,True) -> [(y2z2,x1y2,x1z1),(y2z2,x1z1,y1z2),(y1z2,x1z1,y1z1)]

		(False,False,    True, False,
		 True, False,    True, False) -> [(y2z2,x1y2,x1z1),(y2z2,x1z1,y1z2),(y1z2,x1z1,y1z1)]

		(False,True,     False,True,
		 True, True,     False,True) -> [(y1z2,x1y1,x1z1),(y1z2,x1z1,y2z2),(y2z2,x1z1,y2z1)]

		(True, False,    True, False,
		 False,False,    True, False) -> [(y1z2,x1y1,x1z1),(y1z2,x1z1,y2z2),(y2z2,x1z1,y2z1)]



		-- x flat +1

		(True, True,     True, True,
		 False,False,    True, False) -> [(x1z1,x2z1,x1y1),(x1y1,x2z1,x2z2),(x1y1,x2z2,y1z2)]

		(False,False,    False,False,
		 True, True,     False,True ) -> [(x1z1,x2z1,x1y1),(x1y1,x2z1,x2z2),(x1y1,x2z2,y1z2)]

		(False,False,    True, False,
		 True, True,     True, True) -> [(x1z1,x2z1,x1y2),(x1y2,x2z1,x2z2),(x1y2,x2z2,y2z2)]

		(True, True,     False,True,
		 False,False,    False,False) -> [(x1z1,x2z1,x1y2),(x1y2,x2z1,x2z2),(x1y2,x2z2,y2z2)]

		(True, True,     True, True,
		 False,False,    False,True) -> [(x2z1,x1z1,x2y1),(x2y1,x1z1,x1z2),(x2y1,x1z2,y1z2)]

		(False,False,    False,False,
		 True, True,     True, False) -> [(x2z1,x1z1,x2y1),(x2y1,x1z1,x1z2),(x2y1,x1z2,y1z2)]

		(False,False,    False,True,
		 True, True,     True, True) -> [(x2z1,x1z1,x2y2),(x2y2,x1z1,x1z2),(x2y2,x1z2,y2z2)]

		(True, True,     True, False,
		 False,False,    False,False) -> [(x2z1,x1z1,x2y2),(x2y2,x1z1,x1z2),(x2y2,x1z2,y2z2)]


		(True, True,     True, True,
		 True, False,    False,False) -> [(x1z2,x2z2,x1y1),(x1y1,x2z2,x2z1),(x1y1,x2z1,y1z1)]

		(False,False,    False,False,
		 False,True,     True, True ) -> [(x1z2,x2z2,x1y1),(x1y1,x2z2,x2z1),(x1y1,x2z1,y1z1)]

		(True, False,    False,False,
		 True, True,     True, True ) -> [(x1z2,x2z2,x1y2),(x1y2,x2z2,x2z1),(x1y2,x2z1,y2z1)]

		(False,True,     True, True,
		 False,False,    False,False) -> [(x1z2,x2z2,x1y2),(x1y2,x2z2,x2z1),(x1y2,x2z1,y2z1)]

		(True, True,     True, True,
		 False,True,     False,False) -> [(x2z2,x1z2,x2y1),(x2y1,x1z2,x1z1),(x2y1,x1z1,y1z1)]

		(False,False,    False,False,
		 True, False,    True, True) -> [(x2z2,x1z2,x2y1),(x2y1,x1z2,x1z1),(x2y1,x1z1,y1z1)]

		(False,True,     False,False,
		 True, True,     True, True) -> [(x2z2,x1z2,x2y2),(x2y2,x1z2,x1z1),(x2y2,x1z1,y2z1)]

		(True, False,    True, True,
		 False,False,    False,False) -> [(x2z2,x1z2,x2y2),(x2y2,x1z2,x1z1),(x2y2,x1z1,y2z1)]



		(True, True,     True, False,
		 True, False,    False,False) -> [(x1y1,x1z2,y1z1),(y1z1,x1z2,y2z2),(y1z1,y2z2,x2z1),(x2z1,y2z2,x2y2)]

		(False,False,    False,True,
		 False,True,     True, True ) -> [(x1y1,x1z2,y1z1),(y1z1,x1z2,y2z2),(y1z1,y2z2,x2z1),(x2z1,y2z2,x2y2)]

		(True, True,     False,True,
		 False,True,     False,False) -> [(x2y1,x2z2,y1z1),(y1z1,x2z2,y2z2),(y1z1,y2z2,x1z1),(x1z1,y2z2,x1y2)]

		(False,False,    True, False,
		 True, False,    True, True ) -> [(x2y1,x2z2,y1z1),(y1z1,x2z2,y2z2),(y1z1,y2z2,x1z1),(x1z1,y2z2,x1y2)]




		(True, False,    False,False,
		 True, True,     True, False) -> [(x1y2,x1z2,y2z1),(y2z1,x1z2,y1z2),(y2z1,y1z2,x2z1),(x2z1,y1z2,x2y1)]

		(False,True,     True, True,
		 False,False,    False,True ) -> [(x1y2,x1z2,y2z1),(y2z1,x1z2,y1z2),(y2z1,y1z2,x2z1),(x2z1,y1z2,x2y1)]

		(False,True,     False,False,
		 True, True,     False,True ) -> [(x2y2,x2z2,y2z1),(y2z1,x2z2,y1z2),(y2z1,y1z2,x1z1),(x1z1,y1z2,x1y1)]

		(True, False,    True, True,
		 False,False,    True, False) -> [(x2y2,x2z2,y2z1),(y2z1,x2z2,y1z2),(y2z1,y1z2,x1z1),(x1z1,y1z2,x1y1)]




		_ -> []

