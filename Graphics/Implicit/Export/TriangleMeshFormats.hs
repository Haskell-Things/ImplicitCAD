{-# LANGUAGE OverloadedStrings #-}

-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.TriangleMeshFormats where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.TextBuilderUtils

stl triangles = toLazyText $ stlHeader <> mconcat (map triangle triangles) <> stlFooter
	where
		stlHeader = "solid ImplictCADExport\n"
		stlFooter = "endsolid ImplictCADExport\n"
		vertex :: ℝ3 -> Builder
		vertex (x,y,z) = mconcat ["vertex " 
                                         ,bf x , " "
                                         ,bf y , " " 
                                         ,bf z]
		triangle :: (ℝ3, ℝ3, ℝ3) -> Builder
		triangle (a,b,c) =
	            "facet normal 0 0 0\n"
	            <> "outer loop\n"
	            <> vertex a <> "\n"
	            <> vertex b <> "\n"
	            <> vertex c
		    <> "\nendloop\nendfacet\n"

jsTHREE :: TriangleMesh -> Text
jsTHREE triangles = toLazyText $ header <> vertcode <> facecode <> footer
        where
                -- some dense JS. Let's make helper functions so that we don't repeat code each line
                header = mconcat [
                          "var Shape = function(){\n"
                         ,"var s = this;\n"
                         ,"THREE.Geometry.call(this);\n"
                         ,"function vec(x,y,z){return new THREE.Vector3(x,y,z);}\n"
                         ,"function v(x,y,z){s.vertices.push(vec(x,y,z));}\n"
                         ,"function f(a,b,c){"
                         ,"s.faces.push(new THREE.Face3(a,b,c));"
                         ,"}\n" ]
                footer = mconcat [
                          "}\n"
                         ,"Shape.prototype = new THREE.Geometry();\n"
                         ,"Shape.prototype.constructor = Shape;\n" ]
                -- A vertex line; v (0.0, 0.0, 1.0) = "v(0.0,0.0,1.0);\n"
                v :: ℝ3 -> Builder
                v (x,y,z) = "v(" <> bf x <> "," <> bf y <> "," <> bf z <> ");\n"
                -- A face line
                f :: Int -> Int -> Int -> Builder
                f posa posb posc = 
                        "f(" <> buildInt posa <> "," <> buildInt posb <> "," <> buildInt posc <> ");"
                verts = do
                        -- extract the vertices for each triangle
                        -- recall that a normed triangle is of the form ((vert, norm), ...)
                        (a,b,c) <- triangles
                        -- The vertices from each triangle take up 3 position in the resulting list
                        [a,b,c]
                vertcode = mconcat $ map v verts
                facecode = mconcat $ do
                        (n,_) <- zip [0, 3 ..] triangles
                        let
                                (posa, posb, posc) = (n, n+1, n+2)
                        return $ f posa posb posc