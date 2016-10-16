-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014, 2015, 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.Export.TriangleMeshFormats where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.TextBuilderUtils

import Blaze.ByteString.Builder hiding (Builder)
import Data.ByteString (replicate)
import Data.ByteString.Lazy (ByteString)
import Data.Storable.Endian

import Prelude hiding (replicate)
import Data.VectorSpace
import Data.Cross hiding (normal)

normal :: (ℝ3,ℝ3,ℝ3) -> ℝ3
normal (a,b,c) =
    normalized $ (b + negateV a) `cross3` (c + negateV a)

stl :: [Triangle] -> Text
stl triangles = toLazyText $ stlHeader <> mconcat (map triangle triangles) <> stlFooter
    where
        stlHeader = "solid ImplictCADExport\n"
        stlFooter = "endsolid ImplictCADExport\n"
        vector :: ℝ3 -> Builder
        vector (x,y,z) = bf x <> " " <> bf y <> " " <> bf z
        vertex :: ℝ3 -> Builder
        vertex v = "vertex " <> vector v
        triangle :: (ℝ3, ℝ3, ℝ3) -> Builder
        triangle (a,b,c) =
                "facet normal " <> vector (normal (a,b,c)) <> "\n"
                <> "outer loop\n"
                <> vertex a <> "\n"
                <> vertex b <> "\n"
                <> vertex c
                <> "\nendloop\nendfacet\n"


-- Write a 32-bit little-endian float to a buffer.

-- convert Floats and Doubles to Float.
toFloat :: Real a => a -> Float
toFloat = realToFrac :: (Real a) => a -> Float

float32LE :: Float -> Write
float32LE = writeStorable . LE

binaryStl :: [Triangle] -> ByteString
binaryStl triangles = toLazyByteString $ header <> lengthField <> mconcat (map triangle triangles)
    where header = fromByteString $ replicate 80 0
          lengthField = fromWord32le $ toEnum $ length triangles
          triangle (a,b,c) = normalV (a,b,c) <> point a <> point b <> point c <> fromWord16le 0
          point (x,y,z) = fromWrite $ float32LE (toFloat x) <> float32LE (toFloat y) <> float32LE (toFloat z)
          normalV ps = let (x,y,z) = normal ps
                       in fromWrite $ float32LE (toFloat x) <> float32LE (toFloat y) <> float32LE (toFloat z)

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
