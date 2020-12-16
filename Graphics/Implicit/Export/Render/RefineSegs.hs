-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- export one function, which refines polylines.
module Graphics.Implicit.Export.Render.RefineSegs (refine) where

import Prelude((<), (/), (<>), (*), ($), (&&), (-), (+), (.), (>), abs, sqrt, (<=))

import Graphics.Implicit.Definitions (ℝ, ℝ2, Polyline(Polyline), minℝ, Fastℕ, Obj2)
import Graphics.Implicit.Export.Util (centroid)
import Linear ( Metric(norm, dot), V2(V2), normalize, (^*) )

default (Fastℕ, ℝ)

-- | The purpose of refine is to add detail to a polyline aproximating
--   the boundary of an implicit function and to remove redundant points.
--   We break this into two steps: detail and then simplify.
refine :: ℝ -> Obj2 -> Polyline -> Polyline
refine res obj = simplify res . detail' res obj

-- | We wrap detail to make it ignore very small segments, and to pass in
--   an initial value for a depth counter argument.
-- FIXME: magic number.
detail' :: ℝ -> (ℝ2 -> ℝ) -> Polyline -> Polyline
detail' res obj (Polyline [p1@(V2 x1 y1), p2@(V2 x2 y2)])
  | (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) > res*res/200 = detail 0 res obj $ Polyline [p1,p2]
detail' _ _ a = a

-- | detail adds new points to a polyline to add more detail.
-- FIXME: all of the magic numbers.
detail :: Fastℕ -> ℝ -> (ℝ2 -> ℝ) -> Polyline -> Polyline
detail n res obj (Polyline [p1, p2]) | n < 2 =
    let
        mid = centroid [p1,p2]
        midval = obj mid
    in if abs midval < res / 40
       then Polyline [p1, p2]
       else
         let
           normal = (\(V2 a b) -> V2 b (-a)) $ normalize (p2 - p1)
           derivN = -(obj (mid - (normal ^* (midval/2))) - midval) * (2/midval)
         in
           if abs derivN > 0.5 && abs derivN < 2 && abs (midval/derivN) < 3*res
           then
             let
               mid' = mid - (normal ^* (midval / derivN))
             in
               addPolylines (detail (n+1) res obj (Polyline [p1, mid'])) (detail (n+1) res obj ( Polyline [mid', p2] ))
           else
             let
               derivX = (obj (mid + V2 (res/100) 0) - midval)*100/res
               derivY = (obj (mid + V2 0 (res/100)) - midval)*100/res
               derivNormSq = derivX*derivX + derivY*derivY
             in
               if abs derivNormSq > 0.09 && abs derivNormSq < 4 && abs (midval/sqrt derivNormSq) < 3*res
               then
                 let
                   (dX, dY) = (- derivX*midval/derivNormSq, - derivY*midval/derivNormSq)
                   mid' = mid + V2 dX dY
                   midval' = obj mid'
                   posRatio = midval/(midval + midval')
                   mid'' = mid + V2 (dX*posRatio) (dY*posRatio)
                 in
                   addPolylines (detail (n+1) res obj (Polyline [p1, mid''])) (detail (n+1) res obj ( Polyline [mid'', p2] ))
               else Polyline [p1, p2]

detail _ _ _ x = x

-- FIXME: re-add simplify2 and simplify3?
simplify :: ℝ -> Polyline -> Polyline
simplify _ = {-simplify3 . simplify2 res . -} simplify1

simplify1 :: Polyline -> Polyline
simplify1 (Polyline (a:b:c:xs)) =
    if abs ( ((b - a) `dot` (c - a)) - norm (b - a) * norm (c - a) ) <= minℝ
    then simplify1 (Polyline (a:c:xs))
    else addPolylines (Polyline [a]) (simplify1 (Polyline (b:c:xs)))
simplify1 a = a

addPolylines :: Polyline -> Polyline -> Polyline
addPolylines (Polyline as) (Polyline bs) = Polyline (as <> bs)

{-
simplify2 :: ℝ -> Polyline -> Polyline
simplify2 res [a,b,c,d] =
    if norm (b - c) < res/10
    then [a, ((b + c) / (2::ℝ)), d]
    else [a,b,c,d]
simplify2 _ a = a

simplify3 (a:as) | length as > 5 = simplify3 $ a : half (init as) <> [last as]
    where
        half (a:b:xs) = a : half xs
        half a = a
simplify3 a = a

-}
