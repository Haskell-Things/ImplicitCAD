-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.NormedTriangleMeshFormats where

import Graphics.Implicit.Definitions

obj normedtriangles = text
	where
		-- A vertex line; v (0.0, 0.0, 1.0) = "v 0.0 0.0 1.0\n"
		v :: ℝ3 -> String
		v (x,y,z) = "v "  ++ show x ++ " " ++ show y ++ " " ++ show z ++ "\n"
		-- A normal line; n (0.0, 0.0, 1.0) = "vn 0.0 0.0 1.0\n"
		n :: ℝ3 -> String
		n (x,y,z) = "vn " ++ show x ++ " " ++ show y ++ " " ++ show z ++ "\n"
		verts = do
			-- extract the vertices for each triangle
			-- recall that a normed triangle is of the form ((vert, norm), ...)
			((a,_),(b,_),(c,_)) <- normedtriangles
			-- The vertices from each triangle take up 3 position in the resulting list
			[a,b,c]
		norms = do
			-- extract the normals for each triangle
			((_,a),(_,b),(_,c)) <- normedtriangles
			-- The normals from each triangle take up 3 position in the resulting list
			[a,b,c]
		vertcode = concat $ map v verts
		normcode = concat $ map n norms
		trianglecode = concat $ do
			n <- map ((+1).(*3)) [0,1 .. length normedtriangles -1]
			let
				vta = show  n   -- ++ "//" ++ show  n
				vtb = show (n+1)-- ++ "//" ++ show (n+1)
				vtc = show (n+2)-- ++ "//" ++ show (n+2)
			return $ "f " ++ vta ++ " " ++ vtb ++ " " ++ vtc ++ " " ++ "\n"
		text = vertcode ++ normcode ++ trianglecode

