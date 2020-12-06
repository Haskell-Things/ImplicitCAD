-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2) where

import Prelude(cycle, (/=), uncurry, fst, Eq, zip, drop, const, abs, (-), (/), sqrt, (*), (+), length, fmap, (<=), (&&), (>=), (||), odd, ($), (>), filter, (<), minimum, cos, sin, (.), const, abs, (-), (/), sqrt, (*), (+), length, fmap, (<=), (&&), (>=), (||), odd, ($), (>), filter, (<), minimum, cos, sin, (.))

import Graphics.Implicit.Definitions
    ( SymbolicObj2(..), SharedObj (Empty), Obj2, ℝ2, ℝ )

import Graphics.Implicit.MathUtil
    ( distFromLineSeg, rmaximum, infty )

import Data.VectorSpace ((^-^))
import Data.List (nub)
import Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared)



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
getImplicit2 (SquareR r (dx, dy)) =
    \(x,y) -> rmaximum r [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2]
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
getImplicit2 (PolygonR _ _) = getImplicitShared @SymbolicObj2 Empty
-- (Rounded) CSG
getImplicit2 (Rotate2 θ symbObj) =
    \(x,y) -> let
        obj = getImplicit2 symbObj
    in
        obj ( x*cos θ + y*sin θ, y*cos θ - x*sin θ)
getImplicit2 (Shared2 obj) = getImplicitShared obj

