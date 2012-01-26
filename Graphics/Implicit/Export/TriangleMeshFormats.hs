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

