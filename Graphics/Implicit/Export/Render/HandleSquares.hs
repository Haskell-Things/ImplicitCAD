-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris) where

import Prelude((+), foldMap, (<>), ($), fmap, concat, (.), (==), compare, error, otherwise, concatMap, Bool(..))

import Graphics.Implicit.Definitions (TriangleMesh(TriangleMesh), Triangle(Triangle), AnnotatedTriangleMesh (AnnotatedTriangleMesh, unAnnotatedTriangleMesh), TriangleProvenance(..))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq), AnnotatedTriSquare(AnnotatedTris, AnnotatedSq))
import Linear ( V2(V2), (*^), (^*) )

import GHC.Exts (groupWith)
import Data.List (sortBy)

-- We want small meshes. Essential to this, is getting rid of triangles.
-- We specifically mark quads in tesselation (refer to Graphics.Implicit.
-- Export.Render.Definitions, Graphics.Implicit.Export.Render.TesselateLoops)
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

mergedSquareTris :: [AnnotatedTriSquare TriangleProvenance] -> AnnotatedTriangleMesh TriangleProvenance
mergedSquareTris sqTris =
    let
        -- We don't need to do any work on triangles. They'll just be part of
        -- the list of triangles we give back. So, the triangles coming from
        -- triangles...
        triTriangles :: [(Triangle, TriangleProvenance)]
        triTriangles = [tri | AnnotatedTris tris <- sqTris, tri <- unAnnotatedTriangleMesh tris ]
        -- We actually want to work on the quads, so we find those
        squaresFromTris :: [AnnotatedTriSquare TriangleProvenance]
        squaresFromTris = [ AnnotatedSq x y z q a | AnnotatedSq x y z q a <- sqTris ]

        unmesh (TriangleMesh m) = m

        -- Collect squares that are on the same plane.
        planeAligned = groupWith (\(AnnotatedSq basis z _ _ a) -> (basis,z,a)) squaresFromTris
        -- For each plane:
        -- Select for being the same range on X and then merge them on Y
        -- Then vice versa.
        joined :: [[AnnotatedTriSquare TriangleProvenance]]
        joined = fmap
            ( concatMap joinXaligned . groupWith (\(AnnotatedSq _ _ xS _ _) -> xS)
            . concatMap joinYaligned . groupWith (\(AnnotatedSq _ _ _ yS _) -> yS)
            . concatMap joinXaligned . groupWith (\(AnnotatedSq _ _ xS _ _) -> xS))
            planeAligned
        -- Merge them back together, and we have the desired reult!
        finishedSquares = concat joined

    in
        -- merge them to triangles, and combine with the original triangles.
        AnnotatedTriangleMesh $ triTriangles <> foldMap squareToTri finishedSquares

-- And now for the helper functions that do the heavy lifting...

joinXaligned :: [AnnotatedTriSquare TriangleProvenance] -> [AnnotatedTriSquare TriangleProvenance]
joinXaligned quads@((AnnotatedSq b z xS _ _):_) =
    let
        orderedQuads = sortBy
            (\(AnnotatedSq _ _ _ (V2 ya _) _) (AnnotatedSq _ _ _ (V2 yb _) _) -> compare ya yb)
            quads
        mergeAdjacent (pres@(AnnotatedSq _ _ _ (V2 y1a y2a) a1) : next@(AnnotatedSq _ _ _ (V2 y1b y2b) a2) : others)
          | y2a == y1b = mergeAdjacent (AnnotatedSq b z xS (V2 y1a y2b) (TriangleProvenance_JoinXAligned a1 a2) : others)
          | y1a == y2b = mergeAdjacent (AnnotatedSq b z xS (V2 y1b y2a) (TriangleProvenance_JoinXAligned a1 a2) : others)
          | otherwise  = pres : mergeAdjacent (next : others)
        mergeAdjacent a = a
    in
        mergeAdjacent orderedQuads
joinXaligned (AnnotatedTris _:_) = error "Tried to join y aligned triangles."
joinXaligned [] = []

joinYaligned :: [AnnotatedTriSquare TriangleProvenance] -> [AnnotatedTriSquare TriangleProvenance]
joinYaligned quads@((AnnotatedSq b z _ yS _):_) =
    let
        orderedQuads = sortBy
            (\(AnnotatedSq _ _ (V2 xa _) _ _) (AnnotatedSq _ _ (V2 xb _) _ _) -> compare xa xb)
            quads
        mergeAdjacent (pres@(AnnotatedSq _ _ (V2 x1a x2a) _ a1) : next@(AnnotatedSq _ _ (V2 x1b x2b) _ a2) : others)
          | x2a == x1b = mergeAdjacent (AnnotatedSq b z (V2 x1a x2b) yS (TriangleProvenance_JoinYAligned a1 a2) : others)
          | x1a == x2b = mergeAdjacent (AnnotatedSq b z (V2 x1b x2a) yS (TriangleProvenance_JoinYAligned a1 a2) : others)
          | otherwise  = pres : mergeAdjacent (next : others)
        mergeAdjacent a = a
    in
        mergeAdjacent orderedQuads
joinYaligned (AnnotatedTris _:_) = error "Tried to join y aligned triangles."
joinYaligned [] = []

-- Deconstruct a square into two triangles.
squareToTri :: AnnotatedTriSquare TriangleProvenance -> [(Triangle, TriangleProvenance)]
squareToTri (AnnotatedSq (b1,b2,b3) z (V2 x1 x2) (V2 y1 y2) ann) =
    let
        zV = b3 ^* z
        (x1V, x2V) = (x1 *^ b1, x2 *^ b1)
        (y1V, y2V) = (y1 *^ b2, y2 *^ b2)
        a = zV + x1V + y1V
        b = zV + x2V + y1V
        c = zV + x1V + y2V
        d = zV + x2V + y2V
    in
        [(Triangle (a,b,c), TriangleProvenance_SquareToTri False ann), (Triangle (c,b,d), TriangleProvenance_SquareToTri True ann)]
squareToTri (AnnotatedTris t) = unmesh t
  where
    unmesh (AnnotatedTriangleMesh a) = a

