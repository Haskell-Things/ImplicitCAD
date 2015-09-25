-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.Definitions
import GHC.Exts (groupWith)
import Data.List (sortBy)
import Data.VectorSpace       

-- We want small meshes. Essential to this, is getting rid of triangles.
-- We secifically mark quads in tesselation (refer to Graphics.Implicit.
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

   | joinXaligned (presently disabled)
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

mergedSquareTris :: [TriSquare] -> [Triangle]
mergedSquareTris sqTris = 
    let
        -- We don't need to do any work on triangles. They'll just be part of
        -- the list of triangles we give back. So, the triangles coming from
        -- triangles...
        triTriangles = concat $ map (\(Tris a) -> a) $ filter isTris sqTris 
        -- We actually want to work on the quads, so we find those
        squares = filter (not . isTris) sqTris
        -- Collect ones that are on the same plane.
        planeAligned = groupWith (\(Sq basis z _ _) -> (basis,z)) squares
        -- For each plane:
        -- Select for being the same range on X and then merge them on Y
        -- Then vice versa.
        joined = map 
            ( -- concat . (map joinXaligned) . groupWith (\(Sq _ _ xS _) -> xS)
              concat . (map joinYaligned) . groupWith (\(Sq _ _ _ yS) -> yS)
            . concat . (map joinXaligned) . groupWith (\(Sq _ _ xS _) -> xS)) 
            planeAligned
        -- Merge them back together, and we have the desired reult!
        finishedSquares = concat joined
    in
        -- merge them to triangles, and combine with the original triagneles.
        triTriangles ++ concat (map squareToTri finishedSquares)


-- And now for a bunch of helper functions that do the heavy lifting...

isTris :: TriSquare -> Bool
isTris (Tris _) = True
isTris _ = False


joinXaligned :: [TriSquare] -> [TriSquare]
joinXaligned quads@((Sq b z xS _):_) =
    let
        orderedQuads = sortBy 
            (\(Sq _ _ _ (ya,_)) (Sq _ _ _ (yb,_)) -> compare ya yb)
            quads
        mergeAdjacent (pres@(Sq _ _ _ (y1a,y2a)) : next@(Sq _ _ _ (y1b,y2b)) : others) =
            if y2a == y1b
            then mergeAdjacent ((Sq b z xS (y1a,y2b)): others)
            else if y1a == y2b
            then mergeAdjacent ((Sq b z xS (y1b,y2a)): others)
            else pres : mergeAdjacent (next : others)
        mergeAdjacent a = a
    in
        mergeAdjacent orderedQuads
joinXaligned [] = []

joinYaligned :: [TriSquare] -> [TriSquare]
joinYaligned quads@((Sq b z _ yS):_) =
    let
        orderedQuads = sortBy 
            (\(Sq _ _ (xa,_) _) (Sq _ _ (xb,_) _) -> compare xa xb)
            quads
        mergeAdjacent (pres@(Sq _ _ (x1a,x2a) _) : next@(Sq _ _ (x1b,x2b) _) : others) =
            if x2a == x1b
            then mergeAdjacent ((Sq b z (x1a,x2b) yS): others)
            else if x1a == x2b
            then mergeAdjacent ((Sq b z (x1b,x2a) yS): others)
            else pres : mergeAdjacent (next : others)
        mergeAdjacent a = a
    in
        mergeAdjacent orderedQuads
joinYaligned [] = []


-- Reconstruct a triangle
squareToTri :: TriSquare -> [Triangle]
squareToTri (Sq (b1,b2,b3) z (x1,x2) (y1,y2)) =
    let
        zV = b3 ^* z
        (x1V, x2V) = (x1 *^ b1, x2 *^ b1)
        (y1V, y2V) = (y1 *^ b2, y2 *^ b2)
        a = zV ^+^ x1V ^+^ y1V
        b = zV ^+^ x2V ^+^ y1V
        c = zV ^+^ x1V ^+^ y2V
        d = zV ^+^ x2V ^+^ y2V
    in
        [(a,b,c),(c,b,d)]


