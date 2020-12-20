-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- exports getContourMesh, which returns an array of triangles describing the interior of a 2D object.
module Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh) where

import Prelude(fmap, Bool(True, False), ($), (-), (+), (/), (*), (<=), ceiling, max, div, floor)

import Graphics.Implicit.Definitions (ℕ, ℝ, ℝ2, Polytri(Polytri), Obj2, SymbolicObj2, (⋯/), (⋯*), fromℕtoℝ, fromℕ)

import Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2)

import Graphics.Implicit.ObjectUtil (getBox2)

import Linear ( V2(V2) )

import Data.List(genericIndex)

import Data.Foldable (fold)

-- Each step on the Y axis is done in parallel using Control.Parallel.Strategies
import Control.Parallel.Strategies (using, rdeepseq, parBuffer, parList)
import Graphics.Implicit.Primitives (getImplicit)

-- | Get an array of triangles describing the interior of a 2D object.
getContourMesh :: ℝ2 -> SymbolicObj2 -> [Polytri]
getContourMesh res symObj =
    let
        -- Grow bounds a little to avoid sampling at exact bounds
        (obj, (p1, p2)) = rebound2 (getImplicit symObj, getBox2 symObj)

        -- | How much space are we rendering?
        d = p2 - p1

        -- | How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        n@(V2 nx ny) = ceiling `fmap` (d ⋯/ res)

        -- | A helper for calculating a position inside of the space.
        gridPos :: V2 ℕ -> V2 ℕ -> ℝ2
        gridPos n' m = p1 + d ⋯* ((fromℕtoℝ `fmap` m) ⋯/ (fromℕtoℝ `fmap` n'))

        -- | Alternate Grid mapping funcs
        toGrid :: ℝ2 -> V2 ℕ
        toGrid f = floor `fmap` ((fromℕtoℝ `fmap` n) ⋯* (f - p1) ⋯/ d)

        -- | Evaluate obj on a grid, in parallel.
        valsOnGrid :: [[ℝ]]
        valsOnGrid = [[ obj $ gridPos n (V2 mx my) | mx <- [0..nx-1] ] | my <- [0..ny-1] ] `using` parList rdeepseq

        -- | A faster version of the obj. Sort of like memoization, but done in advance, in parallel.
        preEvaledObj p = valsOnGrid `genericIndex` my `genericIndex` mx where (V2 mx my) = toGrid p

        -- | Compute the triangles.
        trisOnGrid :: [[[Polytri]]]
        trisOnGrid = [[getSquareTriangles (gridPos n (V2 mx my)) (gridPos n (V2 (mx+1) (my+1))) preEvaledObj
             | mx <- [0..nx-1] ] | my <- [0..ny-1] ] `using` parBuffer (max 1 $ fromℕ $ div ny 32) rdeepseq
        -- FIXME: merge adjacent triangles.
        triangles = fold $ fold trisOnGrid
    in
        triangles

-- | This function returns triangles covering a portion of the interior of an object.
--   The triangles are defined in a clockwise fashion, beginning on the point closest to origin, or failing that, the closest point to x1 on the x axis.
--   It is based on the linearly-interpolated marching squares algorithm.
getSquareTriangles :: ℝ2 -> ℝ2 -> Obj2 -> [Polytri]
getSquareTriangles (V2 x1 y1) (V2 x2 y2) obj =
    let
        (V2 x y) = V2 x1 y1

        -- Let's evaluate obj at four corners...
        x1y1 = obj $ V2 x1 y1
        x2y1 = obj $ V2 x2 y1
        x1y2 = obj $ V2 x1 y2
        x2y2 = obj $ V2 x2 y2

        -- And the center point..
        c = obj $ V2 ((x1+x2)/2) ((y1+y2)/2)

        -- Assumptions:
        -- FIXME: interpolations for C?
        -- Evaluating obj is expensive, only evaluate C when it being positive will increase/decrease the surface area by 50% or more.

        dx = x2 - x1
        dy = y2 - y1

        -- linearly interpolated intersections on the relevant axis:
        -- (x1,y1)     inty1     (x2,y1)
        --     *_________*_________*
        --     |                   |
        --     |                   |
        --     |                   |
        --intx1*         *c        *intx2
        --     |                   |
        --     |                   |
        --     |                   |
        --     *---------*---------*
        -- (x1,y2)     inty2     (x2,y2)

        -- These intersections are where the edge of the object crosses the axis, not the middle of the axis.
        intx1 = V2 x                         (y + dy*x1y1/(x1y1-x1y2))
        intx2 = V2 (x + dx)                  (y + dy*x2y1/(x2y1-x2y2))
        inty1 = V2 (x + dx*x1y1/(x1y1-x2y1)) y
        inty2 = V2 (x + dx*x1y2/(x1y2-x2y2)) (y + dy)

        -- decompose a square into two triangles...
        square :: ℝ2 -> ℝ2 -> ℝ2 -> ℝ2 -> [Polytri]
        square aa bb cc dd = [Polytri (aa,bb,cc), Polytri (aa,cc,dd)]

    in case (x1y1 <= 0, x2y1 <= 0, x2y2 <= 0, x1y2 <= 0, c <= 0) of
        -- Yes, there's some symetries that could reduce the amount of code...
        -- But I don't think they're worth exploiting...
        (False, False, False, False,     _) -> []
        ( True,  True,  True,  True,     _) -> square   (V2 x1 y1)(V2 x2 y1)(V2 x2 y2) (V2 x1 y2)
        ( True,  True, False, False,     _) -> square   (V2 x1 y1)(V2 x2 y1)  intx2     intx1
        (False,  True,  True, False,     _) -> square     inty1   (V2 x2 y1)(V2 x2 y2)  inty2
        (False, False,  True,  True,     _) -> square     intx1     intx2   (V2 x2 y2) (V2 x1 y2)
        ( True, False, False,  True,     _) -> square   (V2 x1 y1)  inty1     inty2    (V2 x1 y2)
        (False, False, False,  True,     _) -> [Polytri ( intx1  ,  inty2  , V2 x1 y2)]
        (False, False,  True, False,     _) -> [Polytri ( intx2  , V2 x2 y2,  inty2  )]
        (False,  True, False, False,     _) -> [Polytri ( inty1  , V2 x2 y1,  intx1  )]
        ( True, False, False, False,     _) -> [Polytri (V2 x1 y1,  inty1  ,  intx1  )]
        (False,  True,  True,  True,     _) -> [Polytri ( intx1  , V2 x2 y2, V2 x1 y2), Polytri ( inty1  , V2 x2 y2,  intx1  ), Polytri ( inty1  , V2 x2 y1, V2 x2 y2)]
        ( True, False,  True,  True,     _) -> [Polytri (V2 x1 y1,  inty1  , V2 x1 y2), Polytri ( inty1  ,  intx2  , V2 x1 y2), Polytri ( intx2  , V2 x2 y2, V2 x1 y2)]
        ( True,  True, False,  True,     _) -> [Polytri (V2 x1 y1,  inty2  , V2 x1 y2), Polytri (V2 x1 y1,  intx2  ,  inty2  ), Polytri (V2 x1 y1, V2 x2 y1,  intx2 )]
        ( True,  True,  True, False,     _) -> [Polytri (V2 x1 y1, V2 x2 y1,  intx1  ), Polytri ( intx1  , V2 x2 y1,  inty2  ), Polytri (V2 x2 y1, V2 x2 y2,  inty2 )]
        (False,  True, False,  True, False) -> [Polytri ( inty1  , V2 x1 y2,  intx1  ), Polytri (V2 x2 y1,  inty2  ,  intx2  )]
        (False,  True, False,  True,  True) -> [Polytri ( inty1  , V2 x1 y2,  intx1  ), Polytri (V2 x2 y1,  inty2  ,  intx2  ), Polytri ( inty1  , V2 x2 y1, V2 x1 y2), Polytri (V2 x2 y1,  inty2  , V2 x1 y2)]
        ( True, False,  True, False, False) -> [Polytri (V2 x1 y1,  inty1  ,  intx2  ), Polytri ( intx1  , V2 x2 y2,  inty2  )]
        ( True, False,  True, False,  True) -> [Polytri (V2 x1 y1,  inty1  ,  intx2  ), Polytri ( intx1  , V2 x2 y2,  inty2  ), Polytri (V2 x1 y1,  intx2  , V2 x2 y2), Polytri (V2 x1 y1, V2 x2 y2,  intx1  )]

