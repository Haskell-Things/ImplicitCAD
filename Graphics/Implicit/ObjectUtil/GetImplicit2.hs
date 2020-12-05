{-# LANGUAGE ViewPatterns #-}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2) where

import Prelude(cycle, (/=), uncurry, fst, Eq, zip, drop, (<$>), const, abs, (-), (/), sqrt, (*), (+), length, fmap, (<=), (&&), (>=), (||), odd, ($), (>), filter, (<), minimum, max, cos, sin, (.))

import Graphics.Implicit.Definitions (minℝ, ℝ, ℝ2, (⋯/), Obj2, SymbolicObj2(Empty2, Full2, SquareR, Circle, PolygonR, Complement2, UnionR2, DifferenceR2, IntersectR2, Translate2, Scale2, Rotate2, Mirror2, Shell2, Outset2, EmbedBoxedObj2))

import Graphics.Implicit.MathUtil (rmax, infty, reflect, rminimum, rmaximum, distFromLineSeg)

import Data.VectorSpace ((^-^))
import Data.List (nub)


------------------------------------------------------------------------------
-- | Filter out equal consecutive elements in the list. This function will
-- additionally trim the last element of the list if it's equal to the first.
scanUniqueCircular :: Eq a => [a] -> [a]
scanUniqueCircular
    = fmap fst
    . filter (uncurry (/=))
    . circularPairs


------------------------------------------------------------------------------
-- | Given @[a, b, c, ... n]@, return the pairs @[(a, b), (b, c), ... (n, a)]@.
circularPairs :: [a] -> [(a,a)]
circularPairs as = zip as $ drop 1 $ cycle as


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
getImplicit2 (PolygonR _ (scanUniqueCircular -> points@(_:_:_:_))) =
    \p -> let
        pairs :: [(ℝ2,ℝ2)]
        pairs =  circularPairs points
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
getImplicit2 (PolygonR _ _) = getImplicit2 Empty2
-- (Rounded) CSG
getImplicit2 (Complement2 symbObj) =
    \p -> let
        obj = getImplicit2 symbObj
    in
        - obj p
getImplicit2 (UnionR2 _ []) = getImplicit2 Empty2
getImplicit2 (UnionR2 r symbObjs) =
    \p -> let
        objs = fmap getImplicit2 symbObjs
    in
        rminimum r $ fmap ($p) objs
getImplicit2 (DifferenceR2 _ symbObj []) = getImplicit2 symbObj
getImplicit2 (DifferenceR2 r symbObj symbObjs) =
    let
        tailObjs = getImplicit2 <$> symbObjs
        headObj = getImplicit2 symbObj
        complement :: Obj2 -> ℝ2 -> ℝ
        complement obj' p = - obj' p
    in
      \p -> do
        let
          maxTail = rmaximum r $ fmap ($p) $ complement <$> tailObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail
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
