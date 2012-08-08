-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.TriangleMeshFormats where

import Graphics.Implicit.Definitions

stl triangles = text
	where
		stlHeader = "solid ImplictCADExport\n"
		stlFooter = "endsolid ImplictCADExport\n"
		vertex :: ℝ3 -> String
		vertex (x,y,z) = "vertex " ++ show x ++ " " ++ show y ++ " " ++ show z
		stlTriangle :: (ℝ3, ℝ3, ℝ3) -> String
		stlTriangle (a,b,c) =
			"facet normal 0 0 0\n"
			++ "outer loop\n"
			++ vertex a ++ "\n"
			++ vertex b ++ "\n"
			++ vertex c ++ "\n"
			++ "endloop\n"
			++ "endfacet\n"
		text = stlHeader
			++ (concat $ map stlTriangle triangles)
			++ stlFooter


jsTHREE :: TriangleMesh -> String
jsTHREE triangles = text
	where
		-- some dense JS. Let's make helper functions so that we don't repeat code each line
		header = 
			"var Shape = function(){\n"
			++  "var s = this;\n"
			++  "THREE.Geometry.call(this);\n"
			++  "function vec(x,y,z){return new THREE.Vector3(x,y,z);}\n"
			++  "function v(x,y,z){s.vertices.push(vec(x,y,z));}\n"
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
		f :: Int -> Int -> Int -> String
		f posa posb posc = 
			"f(" ++ show posa ++ "," ++ show posb ++ "," ++ show posc ++ ");"
		verts = do
			-- extract the vertices for each triangle
			-- recall that a normed triangle is of the form ((vert, norm), ...)
			(a,b,c) <- triangles
			-- The vertices from each triangle take up 3 position in the resulting list
			[a,b,c]
		vertcode = concat $ map v verts
		facecode = concat $ do
			n <- [0, 3 ..]
			let
				(posa, posb, posc) = (n, n+1, n+2)
			return $ f posa posb posc
		text = header ++ vertcode ++ facecode ++ footer

