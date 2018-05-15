-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- export getContourMesh, which returns an array of triangles describing the interior of a 2D object.
module Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh) where

import Prelude(Bool(True, False), fromIntegral, ($), (-), (+), (/), (*), (<=), (>), ceiling, concat, max, div)

import Graphics.Implicit.Definitions (ℕ, ℝ2, Polytri, Obj2, (⋯/), (⋯*))

import Data.VectorSpace ((^-^),(^+^))

-- Each step on the Y axis is done in parallel using Control.Parallel.Strategies
import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

-- apply a function to both items in the provided tuple.
both :: forall t b. (t -> b) -> (t, t) -> (b, b)
both f (x,y) = (f x, f y)

getContourMesh :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [Polytri]
getContourMesh p1 p2 res obj =
    let
        -- How much space are we rendering?
        d = p2 ^-^ p1

        -- How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        n@(nx,ny) = (ceiling) `both` (d ⋯/ res)

        -- a helper for calculating a position inside of the space.
        gridPos :: (ℕ,ℕ) -> (ℕ,ℕ) -> ℝ2
        gridPos n' m = p1 ^+^ d ⋯* ((fromIntegral `both` m) ⋯/ (fromIntegral `both` n'))

        -- compute the triangles.
        trisOnGrid :: [[[Polytri]]]
        trisOnGrid = [[getSquareTriangles (gridPos n (mx,my)) (gridPos n (mx+1,my+1)) obj
             | mx <- [0.. nx-1] ] | my <- [0..ny-1] ] `using` parBuffer (max 1 $ fromIntegral $ div ny 32) rdeepseq
        triangles = concat $ concat trisOnGrid
    in
        triangles

-- | This function gives line segments to divide negative interior
--  regions and positive exterior ones inside a square, based on its
--  values at its vertices.
--  It is based on the linearly-interpolated marching squares algorithm.

getSquareTriangles :: ℝ2 -> ℝ2 -> Obj2 -> [Polytri]
getSquareTriangles (x1, y1) (x2, y2) obj =
    let
        (x,y) = (x1, y1)

        -- Let's evaluate obj at four corners...
        x1y1 = obj (x1, y1)
        x2y1 = obj (x2, y1)
        x1y2 = obj (x1, y2)
        x2y2 = obj (x2, y2)

        -- And the center point..
        c = obj ((x1+x2)/2, (y1+y2)/2)

        dx = x2 - x1
        dy = y2 - y1

        -- linearly interpolated midpoints on the relevant axis
        --             midy2
        --      _________*_________
        --     |                   |
        --     |                   |
        --     |                   |
        --midx1*                   * midx2
        --     |                   |
        --     |                   |
        --     |                   |
        --      ---------*---------
        --             midy1

        midx1 = (x,                       y + dy*x1y1/(x1y1-x1y2))
        midx2 = (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
        midy1 = (x + dx*x1y1/(x1y1-x2y1), y )
        midy2 = (x + dx*x1y2/(x1y2-x2y2), y + dy)

        -- decompose a square into two triangles...
        square :: forall t t1. t -> t1 -> t1 -> t1 -> [(t, t1, t1)]
        square aa bb cc dd = [(aa,bb,cc), (aa,cc,dd)]

    in case (x1y2 <= 0, x2y2 <= 0,
             x1y1 <= 0, x2y1 <= 0) of
        -- Yes, there's some symetries that could reduce the amount of code...
        -- But I don't think they're worth exploiting...
        (True,  True,
         True,  True)  -> square (x1,y1) (x2,y1) (x2,y2) (x1,y2)
        (False, False,
         False, False) -> []
        (True,  True,
         False, False) -> square midx1 midx2 (x2,y2) (x1,y2)
        (False, False,
         True,  True)  -> square (x1,y1) (x2,y1) midx2 midx1
        (False, True,
         False, True)  -> square midy1 (x2,y1) (x2,y2) midy2
        (True,  False,
         True,  False) -> square (x1,y1) midy1 midy2 (x1,y2)
        (True,  False,
         False, False) -> [((x1,y2), midx1, midy2)]
        (False, True,
         True,  True)  ->
            [(midx1, (x1,y1), midy2), ((x1,y1), (x2,y1), midy2), (midy2, (x2,y1), (x2,y2))]
        (True,  True,
         False, True)  ->
            [((x1,y2), midx1, (x2,y2)), (midx1, midy1, (x2,y2)), ((x2,y2), midy1, (x2,y1))]
        (False, False,
         True,  False) -> [(midx1, (x1,y1), midy1)]
        (True,  True,
         True,  False) ->
            [(midy1,midx2,(x2,y2)), ((x2,y2), (x1,y2), midy1), (midy1, (x1,y2), (x1,y1))]
        (False, False,
         False, True)  -> [(midx2, midy1, (x2,y1))]
        (True,  False,
         True,  True)  ->
            [(midy2, (x2,y1), midx2), ((x2,y1), midy2, (x1,y1)), ((x1,y1), midy2, (x1,y2))]
        (False, True,
         False, False) -> [(midx2, (x2,y2), midy2)]
        (True,  False,
         False, True)  -> if c > 0
            then [((x1,y2), midx1, midy2), ((x2,y1), midy1, midx2)] --[[midx1, midy2], [midx2, midy1]]
            else [] --[[midx1, midy1], [midx2, midy2]]
        (False, True,
         True,  False) -> if c <= 0
            then [] --[[midx1, midy2], [midx2, midy1]]
            else [((x1,y1), midy1, midx1), ((x2,y2), midx2, midy2)] --[[midx1, midy1], [midx2, midy2]]



