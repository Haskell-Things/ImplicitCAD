-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.MathUtil (rmax, rmin, rmaximum, rminimum, distFromLineSeg, pack, box3sWithin) where

import Data.List
import Data.VectorSpace
import Data.AffineSpace
import Graphics.Implicit.Definitions

-- | The distance a point p is from a line segment (a,b)
distFromLineSeg :: ℝ2 -> (ℝ2,ℝ2) -> ℝ
distFromLineSeg p (a,b) = magnitude (closest .-. p)
    where
        ab = b ^-^ a
        ap = p ^-^ a
        d  = normalized ab ⋅ ap
        closest
            | d < 0 = a
            | d > magnitude ab = b
            | otherwise = a ^+^ d *^ normalized ab

        

box3sWithin :: ℝ -> (ℝ3, ℝ3) -> (ℝ3,ℝ3) -> Bool
box3sWithin r ((ax1, ay1, az1),(ax2, ay2, az2)) ((bx1, by1, bz1),(bx2, by2, bz2)) =
    let
        near (a1, a2) (b1, b2) = not $ (a2 + r < b1) || (b2 + r < a1)
    in
           (ax1,ax2) `near` (bx1, bx2)
        && (ay1,ay2) `near` (by1, by2)
        && (az1,az2) `near` (bz1, bz2)


-- | Rounded Maximum
-- Consider  max(x,y) = 0, the generated curve 
-- has a square-like corner. We replace it with a 
-- quarter of a circle
rmax :: 
    ℝ     -- ^ radius
    -> ℝ  -- ^ first number to round maximum
    -> ℝ  -- ^ second number to round maximum
    -> ℝ  -- ^ resulting number
rmax r x y =  if abs (x-y) < r 
    then y - r*sin(pi/4-asin((x-y)/r/sqrt 2)) + r
    else max x y

-- | Rounded minimum
rmin :: 
    ℝ     -- ^ radius
    -> ℝ  -- ^ first number to round minimum
    -> ℝ  -- ^ second number to round minimum
    -> ℝ  -- ^ resulting number
rmin r x y = if abs (x-y) < r 
    then y + r*sin(pi/4+asin((x-y)/r/sqrt 2)) - r
    else min x y

-- | Like rmax, but on a list instead of two.
-- Just as maximum is.
-- The implementation is to take the maximum two
-- and rmax those.

rmaximum ::
    ℝ      -- ^ radius
    -> [ℝ] -- ^ numbers to take round maximum
    -> ℝ   -- ^ resulting number
rmaximum _ (a:[]) = a
rmaximum r (a:b:[]) = rmax r a b
rmaximum r l = 
    let
        tops = reverse $ sort l
    in
        rmax r (tops !! 0) (tops !! 1)

-- | Like rmin but on a list.
rminimum ::
    ℝ      -- ^ radius
    -> [ℝ] -- ^ numbers to take round minimum
    -> ℝ   -- ^ resulting number
rminimum _ (a:[]) = a
rminimum r (a:b:[]) = rmin r a b
rminimum r l = 
    let
        tops = sort l
    in
        rmin r (tops !! 0) (tops !! 1)


pack :: 
    Box2           -- ^ The box to pack within
    -> ℝ           -- ^ The space seperation between items
    -> [(Box2, a)] -- ^ Objects with their boxes
    -> ([(ℝ2, a)], [(Box2, a)] ) -- ^ Packed objects with their positions, objects that could be packed

pack (dx, dy) sep objs = packSome sortedObjs (dx, dy)
    where
        compareBoxesByY  ((_, ay1), (_, ay2))  ((_, by1), (_, by2)) = 
                compare (abs $ by2-by1) (abs $ ay2 - ay1)

        sortedObjs = sortBy 
            (\(boxa, _) (boxb, _) -> compareBoxesByY boxa boxb ) 
            objs

        tmap1 f (a,b) = (f a, b)
        tmap2 f (a,b) = (a, f b)

        --packSome :: [(Box2,a)] -> Box2 -> ([(ℝ2,a)], [(Box2,a)])
        packSome (presObj@(((x1,y1),(x2,y2)),obj):otherBoxedObjs) box@((bx1, by1), (bx2, by2)) = 
            if abs (x2 - x1) <= abs (bx2-bx1) && abs (y2 - y1) <= abs (by2-by1)
            then 
                let
                    row = tmap1 (((bx1-x1,by1-y1), obj):) $
                        packSome otherBoxedObjs ((bx1+x2-x1+sep, by1), (bx2, by1 + y2-y1))
                    rowAndUp = 
                        if abs (by2-by1) - abs (y2-y1) > sep
                        then tmap1 ((fst row) ++ ) $
                            packSome (snd row) ((bx1, by1 + y2-y1+sep), (bx2, by2))
                        else row
                in
                    rowAndUp
            else
                tmap2 (presObj:) $ packSome otherBoxedObjs box
        packSome [] _ = ([], [])



