-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.RefineSegs where

import Data.VectorSpace
import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Util (centroid)

-- The purpose of refine is to add detail to a polyline aproximating
-- the boundary of an implicit function and to remove redundant points.


-- We break this into two steps: detail and then simplify.

refine :: ℝ -> Obj2 -> [ℝ2] -> [ℝ2]
refine res obj = simplify res . detail' res obj

-- we wrap detail to make it ignore very small segments, and to pass in 
-- an initial value for a pointer counter argument. This is detail'


detail' :: ℝ -> (ℝ2 -> ℝ) -> [ℝ2] -> [ℝ2]
detail' res obj [p1@(x1,y1), p2@(x2,y2)] | (x2-x1)**2 + (y2-y1)**2 > res**2/200 =
        detail 0 res obj [p1,p2]
detail' _ _ a = a

-- detail adds new points to a polyline to add more detail.

detail :: Int -> ℝ -> (ℝ2 -> ℝ) -> [ℝ2] -> [ℝ2]
detail n res obj [p1, p2] | n < 2 =
    let
        mid = centroid [p1,p2]
        midval = obj mid 
    in if abs midval < res / 40
    then [p1, p2]
    else let
        normal = (\(a,b) -> (b, -a)) $ normalized (p2 ^-^ p1) 
        derivN = -(obj (mid ^-^ (normal ^* (midval/2))) - midval) * (2/midval)
    in if abs derivN > 0.5 && abs derivN < 2 && abs (midval/derivN) < 3*res
    then let
        mid' = mid ^-^ (normal ^* (midval / derivN))
    in detail (n+1) res obj [p1, mid'] 
       ++ tail (detail (n+1) res obj [mid', p2] )
    else let
        derivX = (obj (mid ^+^ (res/100, 0)) - midval)*100/res
        derivY = (obj (mid ^+^ (0, res/100)) - midval)*100/res
        derivNormSq = derivX**2 + derivY**2
    in if abs derivNormSq > 0.09 && abs derivNormSq < 4 && abs (midval/sqrt derivNormSq) < 3*res
    then let
        (dX, dY) = (- derivX*midval/derivNormSq, - derivY*midval/derivNormSq)
        mid' = mid ^+^ (dX, dY)
        midval' = obj mid'
        posRatio = midval/(midval - midval')
        mid'' = mid ^+^ (dX*posRatio, dY*posRatio)
    in 
        detail (n+1) res obj [p1, mid''] ++ tail (detail (n+1) res obj [mid'', p2] )
    else [p1, p2]

detail _ _ _ x = x

simplify :: Float -> [ℝ2] -> [ℝ2]
simplify _ = {-simplify3 . simplify2 res . -} simplify1

simplify1 :: [ℝ2] -> [ℝ2]
simplify1 (a:b:c:xs) =
    if abs ( ((b ^-^ a) ⋅ (c ^-^ a)) - magnitude (b ^-^ a) * magnitude (c ^-^ a) ) < 0.0001
    then simplify1 (a:c:xs)
    else a : simplify1 (b:c:xs)
simplify1 a = a

{-
simplify2 :: ℝ -> [ℝ2] -> [ℝ2]
simplify2 res [a,b,c,d] = 
    if norm (b - c) < res/10
    then [a, ((b + c) / (2::ℝ)), d]
    else [a,b,c,d]
simplify2 _ a = a

simplify3 (a:as) | length as > 5 = simplify3 $ a : half (init as) ++ [last as]
    where
        half (a:b:xs) = a : half xs
        half a = a
simplify3 a = a

-}
