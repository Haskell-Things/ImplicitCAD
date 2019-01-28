-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- export getContour, which returns as array of polylines describing the edge of a 2D object.
module Graphics.Implicit.Export.MarchingSquares (getContour) where

import Prelude(Bool(True, False), ceiling, (/), (+), (-), filter, map, ($), (*), (/=), (<=), (>), splitAt, div, unzip, length, (++), (<), (++), head, ceiling, concat, div, max, not, null, (||), Eq, fromIntegral, floor)

import Graphics.Implicit.Definitions (ℕ, ℝ, ℝ2, Polyline, Obj2, (⋯/), (⋯*))

import Data.VectorSpace ((^-^), (^+^))

import Data.List(genericIndex)

import Control.Arrow((***))

-- import a helper, to clean up the result we return.
import Graphics.Implicit.Export.Render.HandlePolylines (reducePolyline)

-- Each step on the Y axis is done in parallel using Control.Parallel.Strategies
import Control.Parallel.Strategies (using, rdeepseq, parBuffer, parList)

-- apply a function to both items in the provided tuple.
both :: forall t b. (t -> b) -> (t, t) -> (b, b)
both f (x,y) = (f x, f y)

-- getContour gets a polyline describing the edge of a 2D object.
getContour :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getContour p1 p2 res obj =
    let
        -- How much space are we rendering?
        d = p2 ^-^ p1

        -- How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        n@(nx,ny) = ceiling `both` (d ⋯/ res)

        -- a helper for calculating a position inside of the space.
        gridPos :: (ℕ,ℕ) -> (ℕ,ℕ) -> ℝ2
        gridPos n' m = p1 ^+^ d ⋯* ((fromIntegral `both` m) ⋯/ (fromIntegral `both` n'))

        -- alternate Grid mapping funcs
        toGrid :: ℝ2 -> (ℕ,ℕ)
        toGrid f = floor `both` ((fromIntegral `both` n) ⋯* (f ^-^ p1) ⋯/ d)

        -- Evaluate obj on a grid, in parallel.
        valsOnGrid :: [[ℝ]]
        valsOnGrid = [[ obj $ gridPos n (mx, my) | mx <- [0..nx-1] ] | my <- [0..ny-1] ] `using` parList rdeepseq

        -- A faster version of the obj. Sort of like memoization, but done in advance, in parallel.
        preEvaledObj p = valsOnGrid `genericIndex` my `genericIndex` mx where (mx,my) = toGrid p

        -- compute the polylines
        linesOnGrid :: [[[Polyline]]]
        linesOnGrid = [[getSquareLineSegs (gridPos n (mx, my)) (gridPos n (mx+1, my+1)) preEvaledObj
             | mx <- [0.. nx-1] ] | my <- [0..ny-1] ] `using` parBuffer (max 1 $ fromIntegral $ div ny 32) rdeepseq

        -- Cleanup, cleanup, everybody cleanup!
        -- (We connect multilines, delete redundant vertices on them, etc)
        lines = filter polylineNotNull $ map reducePolyline $ orderLinesDC linesOnGrid
    in
      lines

-- | This function gives line segments to divide negative interior
--  regions and positive exterior ones inside a square, based on the
--  values at its vertices.
--  It is based on the linearly-interpolated marching squares algorithm.

getSquareLineSegs :: ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getSquareLineSegs (x1, y1) (x2, y2) obj =
    let
        (x,y) = (x1, y1)

        -- Let's evlauate obj at four corners...
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
        --      _________*__________
        --     |                    |
        --     |                    |
        --     |                    |
        --midx1*                    * midx2
        --     |                    |
        --     |                    |
        --     |                    |
        --      ---------*----------
        --             midy1


        midx1 = (x,                       y + dy*x1y1/(x1y1-x1y2))
        midx2 = (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
        midy1 = (x + dx*x1y1/(x1y1-x2y1), y )
        midy2 = (x + dx*x1y2/(x1y2-x2y2), y + dy)

        notPointLine :: Eq a => [a] -> Bool
        notPointLine (start:stop:xs) = start /= stop || notPointLine [stop:xs]
        notPointLine [_] = False
        notPointLine [] = False

    in filter notPointLine $ case (x1y2 <= 0, x2y2 <= 0,
                                   x1y1 <= 0, x2y1 <= 0) of
        -- Yes, there's some symetries that could reduce the amount of code...
        -- But I don't think they're worth exploiting...
        (True,  True,
         True,  True)  -> []
        (False, False,
         False, False) -> []
        (True,  True,
         False, False) -> [[midx1, midx2]]
        (False, False,
         True,  True)  -> [[midx1, midx2]]
        (False, True,
         False, True)  -> [[midy1, midy2]]
        (True,  False,
         True,  False) -> [[midy1, midy2]]
        (True,  False,
         False, False) -> [[midx1, midy2]]
        (False, True,
         True,  True)  -> [[midx1, midy2]]
        (True,  True,
         False, True)  -> [[midx1, midy1]]
        (False, False,
         True,  False) -> [[midx1, midy1]]
        (True,  True,
         True,  False) -> [[midx2, midy1]]
        (False, False,
         False, True)  -> [[midx2, midy1]]
        (True,  False,
         True,  True)  -> [[midx2, midy2]]
        (False, True,
         False, False) -> [[midx2, midy2]]
        (True,  False,
         False, True)  -> if c > 0
            then [[midx1, midy2], [midx2, midy1]]
            else [[midx1, midy1], [midx2, midy2]]
        (False, True,
         True,  False) -> if c <= 0
            then [[midx1, midy2], [midx2, midy1]]
            else [[midx1, midy1], [midx2, midy2]]


-- Functions for cleaning up the polylines
-- Many have multiple implementations as efficiency experiments.
-- At some point, we'll get rid of the redundant ones....

{-
orderLines :: [Polyline] -> [Polyline]
orderLines [] = []
orderLines (present:remaining) =
    let
        findNext ((p3:ps):segs) = if p3 == last present then (Just (p3:ps), segs) else
            if last ps == last present then (Just (reverse $ p3:ps), segs) else
            case findNext segs of (res1,res2) -> (res1,(p3:ps):res2)
        findNext [] = (Nothing, [])
    in
        case findNext remaining of
            (Nothing, _) -> present:(orderLines remaining)
            (Just match, others) -> orderLines $ (present ++ tail match): others
-}

orderLinesDC :: [[[Polyline]]] -> [Polyline]
orderLinesDC segs =
    let
        halve :: [a] -> ([a], [a])
        halve l = splitAt (div (length l) 2) l
        splitOrder segs' = case (halve *** halve) $ unzip $ map halve $ segs' of
            ((a,b),(c,d)) -> orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d
    in
        if length segs < 5 || length (head segs) < 5 then concat $ concat segs else
                splitOrder segs
{-
orderLinesP :: [[[Polyline]]] -> [Polyline]
orderLinesP segs =
    let
        halve l = splitAt (div (length l) 2) l
        splitOrder segs = case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
            ((a,b),(c,d)) -> orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d
        -- force is frome real world haskell
        force xs = go xs `pseq` ()
            where go (_:xs) = go xs
                  go [] = 1
    in
        if (length segs < 5 || length (head segs) < 5 ) then concat $ concat segs else
        case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
            ((a,b),(c,d)) -> orderLines $
                let
                    a' = orderLinesP a
                    b' = orderLinesP b
                    c' = orderLinesP c
                    d' = orderLinesP d
                in (force a' `par` force b' `par` force c' `par` force d') `pseq`
                    (a' ++ b' ++ c' ++ d')
-}


polylineNotNull :: [a] -> Bool
polylineNotNull (_:l) = not (null l)
polylineNotNull [] = False

