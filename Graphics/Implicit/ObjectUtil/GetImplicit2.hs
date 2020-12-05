-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2) where

import Prelude(const, abs, (-), (/), sqrt, (*), (+), mod, length, fmap, (<=), (&&), (>=), (||), odd, ($), (>), filter, (<), minimum, max, cos, sin, tail, (.))

import Graphics.Implicit.Definitions (ℝ, ℕ, ℝ2, (⋯/), Obj2, SymbolicObj2(Empty2, Full2, SquareR, Circle, PolygonR, Complement2, UnionR2, DifferenceR2, IntersectR2, Translate2, Scale2, Rotate2, Mirror2, Shell2, Outset2, EmbedBoxedObj2))

import Graphics.Implicit.MathUtil (infty, reflect, rminimum, rmaximum, distFromLineSeg)

import Data.VectorSpace ((^-^))
import Data.List (nub, genericIndex, genericLength)

getImplicit2 :: SymbolicObj2 -> Obj2
-- Primitives
getImplicit2 Empty2 = const infty
getImplicit2 Full2 = const $ -infty
getImplicit2 (SquareR r (dx, dy)) =
    \(x,y) -> let
    in
         rmaximum r [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2]
getImplicit2 (Circle r) =
    \(x,y) -> sqrt (x * x + y * y) - r
-- FIXME: stop ignoring rounding for polygons.
getImplicit2 (PolygonR _ points) =
    \p -> let
        pair :: ℕ -> (ℝ2,ℝ2)
        pair n = (points `genericIndex` n, points `genericIndex` mod (n + 1) (genericLength points) )
        pairs :: [(ℝ2,ℝ2)]
        pairs =  [ pair n | n <- [0 .. genericLength points - 1] ]
        relativePairs =  fmap (\(a,b) -> (a ^-^ p, b ^-^ p) ) pairs
        crossing_points =
            [x2 ^-^ y2*(x2-x1)/(y2-y1) | ((x1,y1), (x2,y2)) <-relativePairs,
               ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
        -- FIXME: use partition instead?
        seemsInRight = odd . length . filter (>0) $ nub crossing_points
        seemsInLeft = odd . length . filter (<0) $ nub crossing_points
        isIn = seemsInRight && seemsInLeft
        dists :: [ℝ]
        dists = fmap (distFromLineSeg p) pairs
    in
        minimum dists * if isIn then -1 else 1
-- (Rounded) CSG
getImplicit2 (Complement2 symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
    in
        - obj p
getImplicit2 (UnionR2 r symbObjs) =
    \p -> let
        objs = fmap getImplicit2 symbObjs
    in
        rminimum r $ fmap ($p) objs
getImplicit2 (DifferenceR2 _ symbObj []) = getImplicit2 symbObj
getImplicit2 (DifferenceR2 r symbObj symbObjs) =
    let
        objs = fmap getImplicit2 symbObjs
        obj = getImplicit2 symbObj
        complement :: Obj2 -> ℝ2 -> ℝ
        complement obj' p = - obj' p
    in
        \p -> rmaximum r . fmap ($p) $ obj:fmap complement (tail objs)
getImplicit2 (IntersectR2 r symbObjs) =
    \p -> let
        objs = fmap getImplicit2 symbObjs
    in
        rmaximum r $ fmap ($p) objs
-- Simple transforms
getImplicit2 (Translate2 v symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
    in
        obj (p ^-^ v)
getImplicit2 (Scale2 s@(sx,sy) symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
        k = abs $ max sx sy
    in
        k * obj (p ⋯/ s)
getImplicit2 (Rotate2 θ symbObj) =
    \(x,y) -> let
        obj = getImplicit2 symbObj
    in
        obj ( x*cos θ + y*sin θ, y*cos θ - x*sin θ)
getImplicit2 (Mirror2 v symbObj) =
    getImplicit2 symbObj . reflect v
-- Boundary mods
getImplicit2 (Shell2 w symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
    in
        abs (obj p) - w/2
getImplicit2 (Outset2 d symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
    in
        obj p - d
-- Misc
getImplicit2 (EmbedBoxedObj2 (obj,_)) = obj

