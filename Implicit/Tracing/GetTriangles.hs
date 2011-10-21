-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Implicit.Tracing.GetTriangles (getTriangles) where

import Implicit.Definitions


-- This monstrosity of a function gives triangles to divde negative interior
-- regions and positive exterior ones inside a cube, based on its vertices.

-- It is based on the linearly-interpolated marching cubes algorithm.

getTriangles :: ((ℝ,ℝ,ℝ,ℝ,ℝ,ℝ,ℝ,ℝ),ℝ3,ℝ) -> [(ℝ3,ℝ3,ℝ3)]
getTriangles ((x1y1z1,x2y1z1,x1y2z1,x2y2z1,x1y1z2,x2y1z2,x1y2z2,x2y2z2), (x,y,z), d) =
	let
		--{- Linearly interpolated
		x1y1 = (x,   y,   z+d*x1y1z1/(x1y1z1-x1y1z2))
		x1y2 = (x,   y+d, z+d*x1y2z1/(x1y2z1-x1y2z2))
		x2y1 = (x+d, y,   z+d*x2y1z1/(x2y1z1-x2y1z2))
		x2y2 = (x+d, y+d, z+d*x2y2z1/(x2y2z1-x2y2z2))

		x1z1 = (x,   y+d*x1y1z1/(x1y1z1-x1y2z1), z)
		x1z2 = (x,   y+d*x1y1z2/(x1y1z2-x1y2z2), z+d)
		x2z1 = (x+d, y+d*x2y1z1/(x2y1z1-x2y2z1), z)
		x2z2 = (x+d, y+d*x2y1z2/(x2y1z2-x2y2z2), z+d)

		y1z1 = (x+d*x1y1z1/(x1y1z1-x2y1z1), y,   z)
		y1z2 = (x+d*x1y1z2/(x1y1z2-x2y1z2), y,   z+d)
		y2z1 = (x+d*x1y2z1/(x1y2z1-x2y2z1), y+d, z)
		y2z2 = (x+d*x1y2z2/(x1y2z2-x2y2z2), y+d, z+d)
		--}


		{- Non-linearly interpolated
		x1y1 = (x,   y,   z+d/2)
		x1y2 = (x,   y+d, z+d/2)
		x2y1 = (x+d, y,   z+d/2)
		x2y2 = (x+d, y+d, z+d/2)

		x1z1 = (x,   y+d/2, z)
		x1z2 = (x,   y+d/2, z+d)
		x2z1 = (x+d, y+d/2, z)
		x2z2 = (x+d, y+d/2, z+d)

		y1z1 = (x+d/2, y,  z)
		y1z2 = (x+d/2, y,  z+d)
		y2z1 = (x+d/2, y+d,z)
		y2z2 = (x+d/2, y+d,z+d)
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

