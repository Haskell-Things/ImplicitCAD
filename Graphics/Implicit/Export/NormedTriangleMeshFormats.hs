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


jsTHREE :: NormedTriangleMesh -> String
jsTHREE normedtriangles = text
	where
		-- some dense JS. Let's make helper functions so that we don't repeat code each line
		header = 
			"var Shape = function(){\n"
			++  "var s = this;\n"
			++  "THREE.Geometry.call(this);\n"
			++  "function vec(x,y,z){return new THREE.Vector3(x,y,z);}\n"
			++  "function v(x,y,z){s.vertices.push(new THREE.Vertex(vec(x,y,z)));}\n"
			++  "function f(a,b,c){"
			++    "s.faces.push(new THREE.Face3(a,b,c));"
			++  "}\n"
		footer =
			"}\n"
			++ "Shape.prototype = new THREE.Geometry();\n"
			++ "Shape.prototype.constructor = Shape;\n"
		-- A vertex line; v (0.0, 0.0, 1.0) = "v(0.0,0.0,1.0);\n"
		v :: ℝ3 -> String
		v (x,y,z) = "v("  ++ show x ++ "," ++ show y ++ "," ++ show z ++ ");\n"
		-- A face line
		f :: Int -> Int -> Int -> ℝ3 -> ℝ3 -> ℝ3 -> String
		f posa posb posc na@(nax, nay, naz) nb@(nbx, nby, nbz) nc@(ncx, ncy, ncz) = 
			"f(" ++ show posa ++ "," ++ show posb ++ "," ++ show posc ++ ");"
		verts = do
			-- extract the vertices for each triangle
			-- recall that a normed triangle is of the form ((vert, norm), ...)
			((a,_),(b,_),(c,_)) <- normedtriangles
			-- The vertices from each triangle take up 3 position in the resulting list
			[a,b,c]
		vertcode = concat $ map v verts
		facecode = concat $ do
			(n, normedTriangle) <- zip [0, 3 ..] normedtriangles
			let
				(posa, posb, posc) = (n, n+1, n+2)
				((_, na), (_, nb), (_, nc)) = normedTriangle
			return $ f posa posb posc na nb nc
		text = header ++ vertcode ++ facecode ++ footer


