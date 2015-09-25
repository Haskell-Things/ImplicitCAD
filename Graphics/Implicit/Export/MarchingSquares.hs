-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingSquares (getContour) where

import Graphics.Implicit.Definitions
-- FIXME: commented out for now, parallelism is not properly implemented.
-- import Control.Parallel.Strategies (using, parList, rdeepseq)
import Data.VectorSpace

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- | getContour gets a polyline describe the edge of your 2D
--  object. It's really the only function in this file you need
--  to care about from an external perspective.

getContour :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getContour p1 p2 d obj =
    let
        -- How many steps will we take on each axis?
        n@(nx,ny) = (ceiling) `both` ((p2 ^-^ p1) ⋯/ d)
        -- Divide it up and compute the polylines
        gridPos :: (Int,Int) -> (Int,Int) -> ℝ2
        gridPos (nx,ny) (mx,my) = let p = ( fromIntegral mx / fromIntegral nx
                                          , fromIntegral my / fromIntegral ny)
                                  in p1 ^+^ (p2 ^-^ p1) ⋯* p
        linesOnGrid :: [[[Polyline]]]
        linesOnGrid = [[getSquareLineSegs
                   (gridPos n (mx,my))
                   (gridPos n (mx+1,my+1))
                   obj
             | mx <- [0.. nx-1] ] | my <- [0..ny-1] ]
        -- Cleanup, cleanup, everybody cleanup!
        -- (We connect multilines, delete redundant vertices on them, etc)
        multilines = (filter polylineNotNull) $ (map reducePolyline) $ orderLinesDC $ linesOnGrid
    in
        multilines

-- FIXME: Commented out, not used?
{-
getContour2 :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getContour2 p1@(x1, y1) p2@(x2, y2) d obj = 
    let
        -- How many steps will we take on each axis?
        n@(nx,ny) = (fromIntegral . ceiling) `both` ((p2 ^-^ p1) ⋯/ d)
        -- Grid mapping funcs
        fromGrid (mx, my) = let p = (mx/nx, my/ny)
                            in (p1 ^+^ (p2 ^-^ p1) ⋯/ p)
        toGrid (x,y) = (floor $ nx*(x-x1)/(x2-x1), floor $ ny*(y-y1)/(y2-y1))
        -- Evaluate obj on a grid, in parallel.
        valsOnGrid :: [[ℝ]]
        valsOnGrid = [[ obj (fromGrid (mx, my)) | mx <- [0.. nx-1] ] | my <- [0..ny-1] ]
                      `using` parList rdeepseq
        -- A faster version of the obj. Sort of like memoization, but done in advance, in parallel.
        preEvaledObj p = valsOnGrid !! my !! mx where (mx,my) = toGrid p
        -- Divide it up and compute the polylines
        linesOnGrid :: [[[Polyline]]]
        linesOnGrid = [[getSquareLineSegs (fromGrid (mx, my)) (fromGrid (mx+1, my+1)) preEvaledObj
             | mx <- [0.. nx-1] ] | my <- [0..ny-1] ]
        -- Cleanup, cleanup, everybody cleanup!
        -- (We connect multilines, delete redundant vertices on them, etc)
        multilines = (filter polylineNotNull) $ (map reducePolyline) $ orderLinesDC $ linesOnGrid
    in
        multilines
-}

-- | This function gives line segments to divide negative interior
--  regions and positive exterior ones inside a square, based on its 
--  values at its vertices.
--  It is based on the linearly-interpolated marching squares algorithm.

getSquareLineSegs :: ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getSquareLineSegs (x1, y1) (x2, y2) obj =
    let 
        (x,y) = (x1, y1)

        -- Let's evlauate obj at a few points...
        x1y1 = obj (x1, y1)
        x2y1 = obj (x2, y1)
        x1y2 = obj (x1, y2)
        x2y2 = obj (x2, y2)
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
        --     -----------*----------
        --              midy1

        midx1 = (x,                       y + dy*x1y1/(x1y1-x1y2))
        midx2 = (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
        midy1 = (x + dx*x1y1/(x1y1-x2y1), y )
        midy2 = (x + dx*x1y2/(x1y2-x2y2), y + dy)
        notPointLine (p1:p2:[]) = p1 /= p2
    in filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
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



-- $ Functions for cleaning up the polylines
-- Many have multiple implementations as efficiency experiments.
-- At some point, we'll get rid of the redundant ones....


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

reducePolyline ((x1,y1):(x2,y2):(x3,y3):others) = 
    if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):(x3,y3):others) else
    if abs ( (y2-y1)/(x2-x1) - (y3-y1)/(x3-x1) ) < 0.0001 
       || ( (x2-x1) == 0 && (x3-x1) == 0 && (y2-y1)*(y3-y1) > 0)
    then reducePolyline ((x1,y1):(x3,y3):others)
    else (x1,y1) : reducePolyline ((x2,y2):(x3,y3):others)
reducePolyline ((x1,y1):(x2,y2):others) = 
    if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):others) else (x1,y1):(x2,y2):others
reducePolyline l = l


orderLinesDC :: [[[Polyline]]] -> [Polyline]
orderLinesDC segs =
    let
        halve :: [a] -> ([a], [a])
        halve l = splitAt (div (length l) 2) l
        splitOrder segs = case (\(x,y) -> (halve x, halve y)) . unzip . map (halve) $ segs of
            ((a,b),(c,d)) -> orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d
    in
        if (length segs < 5 || length (head segs) < 5 ) then concat $ concat segs else
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


polylineNotNull (_:l) = not (null l)
polylineNotNull [] = False

