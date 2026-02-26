-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE LambdaCase #-}

module Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris) where

import Prelude(abs, show, until, (<$>), (&&), (-), (<), foldMap, (<>), ($), concat, (.), (==), compare, error, otherwise, concatMap)

import Graphics.Implicit.Definitions (ℝ, TriangleMesh(TriangleMesh, getTriangles), Triangle(Triangle))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq))

-- Our linear algebra library.
import Linear (distance, V2(V2))

import GHC.Exts (groupWith)
import Data.List (sortBy)

-- We want small meshes. Essential to accomplishing this, is getting rid of triangles.
-- We specifically mark quads in tesselation (refer to Graphics.Implicit.Export.Render.Definitions, Graphics.Implicit.Export.Render.TesselateLoops)
-- So that we can try and merge them together.

{- Core idea of mergedSquareTris:

  Many Quads on Plane
   ____________
  |    |    |  |
  |____|____|  |
  |____|____|__|

   | joinXaligned
   v
   ____________
  |         |  |
  |_________|__|
  |_________|__|

   | joinYaligned
   v
   ____________
  |         |  |
  |         |  |
  |_________|__|

   | joinXaligned
   v
   ____________
  |            |
  |            |
  |____________|

   | squareToTri
   v
   ____________
  |\           |
  | ---------- |
  |___________\|

-}

mergedSquareTris :: [TriSquare] -> TriangleMesh
mergedSquareTris sqTris =
    let
        -- We don't need to do any work on triangles. They'll just be part of
        -- the list of triangles we give back. So, the triangles coming from
        -- triangles...
        triTriangles :: [Triangle]
        triTriangles = [tri | Tris tris <- sqTris, tri <- getTriangles tris ]
        -- We actually want to work on the quads, so we find those
        squaresFromTris :: [TriSquare]
        squaresFromTris = [ Sq x y z q p | Sq x y z q p <- sqTris ]

        -- Collect squares that are on the same plane.
        planeAligned = groupWith
          (\case
            (Sq basis z _ _ _) -> (basis,z)
            (Tris _) -> error "Unexpected Tris"
          ) squaresFromTris

        -- For each plane:
        -- Select for being the same range on X and then merge them on Y
        -- Then repeat.
        finishedSquares :: [TriSquare]
        finishedSquares = concat $ until (\xs -> attemptJoin xs == xs) attemptJoin <$> planeAligned
        -- Merge them back together, and we have the desired reult!
        attemptJoin :: [TriSquare] -> [TriSquare]
        attemptJoin = concatMap joinYaligned . groupWith
                      (\case
                          (Sq _ _ _ yS _) -> yS
                          (Tris _) -> error "Unexpected Tris"
                      )
                      . concatMap joinXaligned . groupWith
                      (\case
                          (Sq _ _ xS _ _) -> xS
                          (Tris _) -> error "Unexpected Tris"
                      )
    in
        -- merge them to triangles, and combine with the original triangles.
        TriangleMesh $ triTriangles <> foldMap squareToTri finishedSquares

-- Join two X aligned squares.
joinXaligned :: [TriSquare] -> [TriSquare]
joinXaligned quads@((Sq b z xS _ _):_) = mergeAdjacent orderedQuads
  where
        orderedQuads = sortBy
            (\i j -> case (i, j) of
                (Sq _ _ _ (V2 ya _) _, Sq _ _ _ (V2 yb _) _) -> compare ya yb
                _ -> error "Unexpected Tris"
            )
            quads
        mergeAdjacent :: [TriSquare] -> [TriSquare]
        mergeAdjacent (pres@(Sq _ _ _ (V2 y1a y2a) (pa1, pa2, pa3, pa4)) : next@(Sq _ _ _ (V2 y1b y2b) (pb1, pb2, pb3, pb4)) : others)
            -- Merge two squares, sharing an edge, with approximately the same angle.
          | y2a ~= y1b && pa3 .= pb2 && pa4 .= pb1 = mergeAdjacent (Sq b z xS (V2 y1a y2b) (pa1, pa2, pb3, pb4) : others)
          -- Note: we used to have two cases here, was the other one just not needed?
          | y1a ~= y2b = error $ "Other path chosen.\n" <> show pres <> "\n" <> show next <> "\n"
          | otherwise  = pres : mergeAdjacent (next : others)
          where
            (~=) v w = abs (v-w) < eps
            (.=) v w = distance v w < eps
            eps :: ℝ
            eps = 1e-6
        mergeAdjacent a = a
joinXaligned (Tris _:_) = error "Tried to join y aligned triangles."
joinXaligned [] = []

-- Join two Y aligned squares.
joinYaligned :: [TriSquare] -> [TriSquare]
joinYaligned quads@((Sq b z _ yS _):_) = mergeAdjacent orderedQuads
  where
        orderedQuads = sortBy
            (\i j -> case (i, j) of
                (Sq _ _ (V2 xa _) _ _, Sq _ _ (V2 xb _) _ _) -> compare xa xb
                _ -> error "Unexpected Tris"
            )
            quads
        mergeAdjacent :: [TriSquare] -> [TriSquare]
        mergeAdjacent (pres@(Sq _ _ (V2 x1a x2a) _ (pa1, pa2, pa3, pa4)) : next@(Sq _ _ (V2 x1b x2b) _ (pb1, pb2, pb3, pb4)) : others)
          -- Note: we used to have two cases here, was the other one just not needed?
          | x2a ~= x1b && pa1 .= pb2 && pb3 .= pa4 = mergeAdjacent (Sq b z (V2 x1a x2b) yS (pb1, pa2, pa3, pb4) : others)
          | x1a ~= x2b = error $ "Other path chosen.\n" <> show pres <> "\n" <> show next <> "\n"
          | otherwise  = pres : mergeAdjacent (next : others)
          where
            (~=) v w = abs (v-w) < eps
            (.=) v w = distance v w < eps
            eps :: ℝ
            eps = 1e-6
        mergeAdjacent a = a
joinYaligned (Tris _:_) = error "Tried to join y aligned triangles."
joinYaligned [] = []

-- Deconstruct a square into two triangles.
squareToTri :: TriSquare -> [Triangle]
squareToTri (Sq _ _ _ _ (a,b,c,d)) = [Triangle (a,b,c), Triangle (a,c,d)]
squareToTri (Tris t) = getTriangles t

