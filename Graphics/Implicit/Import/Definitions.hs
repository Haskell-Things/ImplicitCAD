{- ORMOLU_DISABLE -}
-- Originally from HSlice. Now a part of ImplicitCAD.
{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019-2026 Julia Longtin
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

-- The top module of our STL handling routines.

-- Allow us to use string literals for ByteStrings
{-# LANGUAGE OverloadedStrings #-}

{- https://www.utgjiu.ro/rev_mec/mecanica/pdf/2010-01/13_Catalin%20Iancu.pdf -}

module Graphics.Implicit.Import.Definitions (trianglesFromSTL) where

import Prelude (($), (==), error, (<$>), (<>), show)

import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Data.ByteString (ByteString)

import Data.ByteString.Char8 (lines, words, unpack, breakSubstring, break, null, drop)

import Text.Read (readMaybe)

import Graphics.Implicit.Definitions (ℝ3,Fastℕ, fromFastℕ)

import Graphics.Implicit.TriUtil (Triangle)

import Linear (V3(V3))

----------------------------------------------------------------
----------- Functions to deal with ASCII STL reading -----------
----------------------------------------------------------------

-- FIXME: ensure we account for https://github.com/openscad/openscad/issues/2651
-- FIXME: handle upper case files.

-- FIXME: support binary STL reading / writing.
{- from the OpenSCAD folks:
15:49 < InPhase> juri_: The binary standard I followed came from this official standards documenting body:  https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL  ;)
15:52 < InPhase> juri_: For example, I followed the "assumed to be little-endian, although this is not stated in documentation", enforcing that with a conversion in the event OpenSCAD is used on a big-endian system.
15:53 < InPhase> Such a constraint seems essential or else it cannot function as a document exchange format.
-}

-- | produce a list of Triangles from the input STL file.
trianglesFromSTL :: Fastℕ -> ByteString -> [Triangle]
trianglesFromSTL threads stl = [readTriangle f | f <- rawTrianglesFromSTL strippedStl] `using` parBuffer (fromFastℕ threads) rdeepseq
  where
    -- strip the first line header off of the stl file.
    (_, headStrippedStl) = break (== '\n') stl
    -- and the last line terminator off of the stl file.
    (strippedStl, _) = breakSubstring "endsolid" headStrippedStl

-- | Separate the STL file into triangles
rawTrianglesFromSTL :: ByteString -> [ByteString]
rawTrianglesFromSTL l = if null l then [] else f : rawTrianglesFromSTL (drop 1 remainder)
    where (f, r) = breakSubstring "endfacet" l
          (_ , remainder) = break (=='\n') r

-- | Read a point when it's given as a string of the form "vertex x y z".
--   Skip any other line.
readVertex :: ByteString -> Maybe ℝ3
readVertex s = readVertex' $ words s
  where
    readVertex' :: [ByteString] -> Maybe ℝ3
    readVertex' [vertex,xs,ys,zs]
      | vertex == "vertex" = case (readMaybe $ unpack xs, readMaybe $ unpack ys, readMaybe $ unpack zs) of
                               (Just x, Just y, Just z) -> Just $ V3 x y z
                               (_maybex,_maybey,_maybez) -> error "error reading."
    readVertex' _ = Nothing

-- | Read a list of three vertexes and generate a triangle from them.
readTriangle :: ByteString -> Triangle
readTriangle f = do
        let
          points = readVertex <$> lines f
          foundPoints = catMaybes points
          triangleFromPoints :: ℝ3 -> ℝ3 -> ℝ3 -> Triangle
          triangleFromPoints p1 p2 p3 = (p1,p2,p3)
        case foundPoints of
          [] -> error $ "no points found" <> show f <> "\n"
          [p1,p2,p3] -> triangleFromPoints p1 p2 p3
          (_a:_b) -> error $ "wrong number of points found." <> show f <> "\n" <> show foundPoints <> "\n"

