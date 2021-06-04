{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2) where

import Prelude(cycle, (/=), uncurry, fst, Eq, zip, drop, abs, (-), (/), sqrt, (*), (+), length, fmap, (<=), (&&), (>=), (||), odd, ($), (>), filter, (<), minimum, (.), sin, cos)

import Graphics.Implicit.Definitions
    ( objectRounding, ObjectContext, SymbolicObj2(Square, Circle, Polygon, Rotate2, Shared2), SharedObj (Empty), Obj2, ℝ2, ℝ )

import Graphics.Implicit.MathUtil
    ( distFromLineSeg, rmaximum )

import Data.List (nub)
import Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared)
import Linear (V2(V2))



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

getImplicit2 :: ObjectContext -> SymbolicObj2 -> Obj2
-- Primitives
getImplicit2 ctx (Square (V2 dx dy)) =
    \(V2 x y) -> rmaximum (objectRounding ctx) [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2]
getImplicit2 _ (Circle r) =
    \(V2 x y) -> sqrt (x * x + y * y) - r
-- FIXME: stop ignoring rounding for polygons.
getImplicit2 _ (Polygon (scanUniqueCircular -> points@(_:_:_:_))) =
    \p -> let
        pairs :: [(ℝ2,ℝ2)]
        pairs =  circularPairs points
        relativePairs =  fmap (\(a,b) -> (a - p, b - p) ) pairs
        crossing_points =
            [x2 - y2*(x2-x1)/(y2-y1) | (V2 x1 y1, V2 x2 y2) <- relativePairs,
               ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
        -- FIXME: use partition instead?
        seemsInRight = odd . length . filter (>0) $ nub crossing_points
        seemsInLeft = odd . length . filter (<0) $ nub crossing_points
        isIn = seemsInRight && seemsInLeft
        dists :: [ℝ]
        dists = fmap (distFromLineSeg p) pairs
    in
        minimum dists * if isIn then -1 else 1
getImplicit2 ctx (Polygon _) = getImplicitShared @SymbolicObj2 ctx Empty
-- (Rounded) CSG
getImplicit2 ctx (Rotate2 θ symbObj) =
    \(V2 x y) -> let
        obj = getImplicit2 ctx symbObj
    in
        obj $ V2 (x*cos θ + y*sin θ) (y*cos θ - x*sin θ)
getImplicit2 ctx (Shared2 obj) = getImplicitShared ctx obj
