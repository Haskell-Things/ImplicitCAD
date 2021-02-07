-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- The purpose of this function is to symbolicaly compute triangle meshes using the symbolic system where possible.
-- Otherwise we coerce it into an implicit function and apply our modified marching cubes algorithm.

module Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh, symbolicGetAnnotatedMesh) where

import Prelude(pure, zip, length, filter, (>), ($), null, (<>), foldMap, (.), (<$>), unlines, zipWith, show, map, snd)

import Graphics.Implicit.Definitions (ℝ, ℝ3, SymbolicObj3(Shared3), SharedObj(UnionR), Triangle, TriangleMesh(TriangleMesh), AnnotatedTriangleMesh(AnnotatedTriangleMesh,unAnnotatedTriangleMesh), removeTriangleMeshAnnotations, TriangleProvenance)
import Graphics.Implicit.Export.Render (getAnnotatedMesh)
import Graphics.Implicit.ObjectUtil (getBox3)
import Graphics.Implicit.MathUtil(box3sWithin)

import Control.Arrow(first, second)

import Debug.Trace

symbolicGetMesh :: ℝ -> SymbolicObj3 -> TriangleMesh
symbolicGetMesh res inputObj = removeTriangleMeshAnnotations $ trace annotations mesh
  where mesh = symbolicGetAnnotatedMesh res inputObj
        annotations = unlines $ zipWith (\n a -> show n <> "\t" <> show a) [1..] $ map snd $ unAnnotatedTriangleMesh mesh

symbolicGetAnnotatedMesh :: ℝ -> SymbolicObj3 -> AnnotatedTriangleMesh TriangleProvenance
symbolicGetAnnotatedMesh res inputObj@(Shared3 (UnionR r objs)) = AnnotatedTriangleMesh $
    let
        boxes = getBox3 <$> objs
        boxedObjs = zip boxes objs

        sepFree :: [((ℝ3, ℝ3), a)] -> ([a], [a])
        sepFree ((box,obj):others) =
            if length (filter (box3sWithin r box) boxes) > 1
            then first  (obj : ) $ sepFree others
            else second (obj : ) $ sepFree others
        sepFree [] = ([],[])

        (dependants, independents) = sepFree boxedObjs
    in if null independents
          then unAnnotatedTriangleMesh $ getAnnotatedMesh (pure res) inputObj
          else if null dependants
                  then foldMap (unAnnotatedTriangleMesh . symbolicGetAnnotatedMesh res) independents
                  else foldMap (unAnnotatedTriangleMesh . symbolicGetAnnotatedMesh res) independents
                       <> unAnnotatedTriangleMesh (symbolicGetAnnotatedMesh res (Shared3 (UnionR r dependants)))

-- | If all that fails, coerce and apply marching cubes :(
symbolicGetAnnotatedMesh res obj = getAnnotatedMesh (pure res) obj

unmesh :: TriangleMesh -> [Triangle]
unmesh (TriangleMesh m) = m
