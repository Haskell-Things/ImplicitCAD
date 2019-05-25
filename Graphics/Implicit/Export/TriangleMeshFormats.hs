-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014, 2015, 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Make string litearls more polymorphic, so we can use them with Builder.
{-# LANGUAGE OverloadedStrings #-}

-- This module exposes three functions, which convert a triangle mesh to an output file.
module Graphics.Implicit.Export.TriangleMeshFormats (stl, binaryStl, jsTHREE) where

import Prelude (Float, Eq, Bool, ($), (+), map, (.), toEnum, length, zip, return, (==), (||), (&&), filter, not)

import Graphics.Implicit.Definitions (Triangle, TriangleMesh, ℕ, ℝ, ℝ3, normalizeℝ3, fromℝtoFloat)

import Graphics.Implicit.Export.TextBuilderUtils (Text, Builder, toLazyText, (<>), bf, buildℕ)

import Blaze.ByteString.Builder (Write, writeStorable, toLazyByteString, fromByteString, fromWord32le, fromWord16le, fromWrite)
import qualified Data.ByteString.Builder.Internal as BI (Builder)

-- note: moved to prelude in newer version
import Data.Monoid(mconcat)

import Data.ByteString (replicate)
import Data.ByteString.Lazy (ByteString)
import Data.Storable.Endian (LittleEndian(LE))

import Data.VectorSpace ((^-^))
import Data.Cross (cross3)

normal :: (ℝ3,ℝ3,ℝ3) -> ℝ3
normal (a,b,c) =
    normalizeℝ3 $ (b ^-^ a) `cross3` (c ^-^ a)

-- | Removes triangles that are empty when converting their positions to Float resolution.
cleanupTris :: TriangleMesh -> TriangleMesh
cleanupTris tris =
    let
        floatPoint :: (ℝ, ℝ, ℝ) -> (Float, Float, Float)
        floatPoint (a,b,c) = (toFloat a, toFloat b, toFloat c)

{-

   -- Alternate methods of detecting degenerate triangles -- not used.
   -- If you have to use one of these, please tell the maintainer.

        -- | Does this triangle fail because it's points are on the same line?
        isDegenerateTriLine (p1,p2,p3) = (norm (p1,p2)) == (norm (p2,p3)) || (norm (p1,p3)) == (norm(p2,p3))
          where
            norm :: ((Float,Float,Float),(Float,Float,Float)) -> (Float,Float,Float)
            norm (begin, end) = normalized $ begin ^-^ end
        -- | Does this triangle fail because of two of it's points overlap?
        isDegenerateTriPoint :: Eq t => (t,t,t) -> Bool
        isDegenerateTriPoint (a,b,c) = (a == b) || (b == c) || (a == c)

-}

        -- | Does this triangle fail because it is constrained on two axises?
        isDegenerateTri2Axis :: forall a. (Eq a) => ((a, a, a),(a, a, a),(a, a, a)) -> Bool
        isDegenerateTri2Axis tri = ((ysame tri) && (xsame tri)) || ((zsame tri) && (ysame tri)) || ((zsame tri) && (xsame tri))
          where
            same :: forall a. Eq a => (a, a, a) -> Bool
            same (n1, n2, n3) = n1 == n2 && n2 == n3
            xsame :: forall a. Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            xsame ((x1,_,_),(x2,_,_),(x3,_,_)) = same (x1, x2, x3)
            ysame :: forall a. Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            ysame ((_,y1,_),(_,y2,_),(_,y3,_)) = same (y1, y2, y3)
            zsame :: forall a. Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            zsame ((_,_,z1),(_,_,z2),(_,_,z3)) = same (z1, z2, z3)
        isDegenerateTri :: Triangle -> Bool
        isDegenerateTri (a, b, c) = (isDegenerateTri2Axis $ floatTri)  -- || (isDegenerateTriLine $ floatTri) || (isDegenerateTriPoint $ floatTri) 
          where
            floatTri = (floatPoint a, floatPoint b, floatPoint c)
    in filter (not . isDegenerateTri) tris

-- | Generate an STL file is ASCII format.
stl :: TriangleMesh -> Text
stl triangles = toLazyText $ stlHeader <> mconcat (map triangle $ cleanupTris triangles) <> stlFooter
    where
        stlHeader :: Builder
        stlHeader = "solid ImplictCADExport\n"
        stlFooter :: Builder
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

-- | convert from ℝ to Float.
toFloat :: ℝ -> Float
toFloat = fromℝtoFloat

-- | Write a 32-bit little-endian float to a buffer.
float32LE :: Float -> Write
float32LE = writeStorable . LE

-- | Generate an STL file in it's binary format.
binaryStl :: TriangleMesh -> ByteString
binaryStl triangles = toLazyByteString $ header <> lengthField <> mconcat (map triangle $ cleanupTris triangles)
    where header = fromByteString $ replicate 80 0
          lengthField = fromWord32le $ toEnum $ length $ cleanupTris triangles
          triangle (a,b,c) = normalV (a,b,c) <> point a <> point b <> point c <> fromWord16le 0
          point :: (ℝ3) -> BI.Builder
          point (x,y,z) = fromWrite $ float32LE (toFloat x) <> float32LE (toFloat y) <> float32LE (toFloat z)
          normalV ps = point $ normal ps

jsTHREE :: TriangleMesh -> Text
jsTHREE triangles = toLazyText $ header <> vertcode <> facecode <> footer
        where
                -- some dense JS. Let's make helper functions so that we don't repeat code each line
                header :: Builder
                header = mconcat [
                          "var Shape = function(){\n"
                         ,"var s = this;\n"
                         ,"THREE.Geometry.call(this);\n"
                         ,"function vec(x,y,z){return new THREE.Vector3(x,y,z);}\n"
                         ,"function v(x,y,z){s.vertices.push(vec(x,y,z));}\n"
                         ,"function f(a,b,c){"
                         ,"s.faces.push(new THREE.Face3(a,b,c));"
                         ,"}\n" ]
                footer :: Builder
                footer = mconcat [
                          "}\n"
                         ,"Shape.prototype = new THREE.Geometry();\n"
                         ,"Shape.prototype.constructor = Shape;\n" ]
                -- A vertex line; v (0.0, 0.0, 1.0) = "v(0.0,0.0,1.0);\n"
                v :: ℝ3 -> Builder
                v (x,y,z) = "v(" <> bf x <> "," <> bf y <> "," <> bf z <> ");\n"
                -- A face line
                f :: ℕ -> ℕ -> ℕ -> Builder
                f posa posb posc =
                        "f(" <> buildℕ posa <> "," <> buildℕ posb <> "," <> buildℕ posc <> ");"
                verts = do
                        -- extract the vertices for each triangle
                        -- recall that a normed triangle is of the form ((vert, norm), ...)
                        (a,b,c) <- cleanupTris triangles
                        -- The vertices from each triangle take up 3 position in the resulting list
                        [a,b,c]
                vertcode = mconcat $ map v verts
                facecode = mconcat $ do
                        (n,_) <- zip [0, 3 ..] $ cleanupTris triangles
                        let
                            (posa, posb, posc) = (n, n+1, n+2) :: (ℕ, ℕ, ℕ)
                        return $ f posa posb posc
