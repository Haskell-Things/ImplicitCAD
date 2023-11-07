{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- A module of math utilities.
module Graphics.Implicit.MathUtil (rmax, rmaximum, rminimum, distFromLineSeg, pack, box3sWithin, reflect, alaV3, packV3, unpackV3, quaternionToEuler, infty) where

-- Explicitly include what we need from Prelude.
import Prelude (Num, Fractional, Bool, RealFloat, Ordering, (.), (>), (<), (+), ($), (/), otherwise, not, (||), (&&), abs, (-), (*), sin, asin, pi, max, sqrt, min, compare, (<=), fst, snd, (<>), flip, (>=), signum, atan2, error, (/=))

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, Box2)

import Data.List (sort, sortBy)
import Linear (Metric, (*^), norm, distance, normalize, dot, Quaternion(Quaternion), V2(V2), V3(V3))

-- | The distance a point p is from a line segment (a,b)
distFromLineSeg :: ℝ2 -> (ℝ2, ℝ2) -> ℝ
distFromLineSeg p (a,b) = distance p closest
    where
        ab = b - a
        ap = p - a
        d :: ℝ
        d  = normalize ab `dot` ap
        -- the closest point to p on the line segment.
        closest :: ℝ2
        closest
            | d < 0 = a
            | d > norm ab = b
            | otherwise = a + d *^ normalize ab

box3sWithin :: ℝ -> (ℝ3, ℝ3) -> (ℝ3, ℝ3) -> Bool
box3sWithin r (V3 ax1 ay1 az1, V3 ax2 ay2 az2) (V3 bx1 by1 bz1, V3 bx2 by2 bz2) =
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
--
-- NOTE: rmax is not associative!
rmax ::
    ℝ     -- ^ radius
    -> ℝ  -- ^ first number to round maximum
    -> ℝ  -- ^ second number to round maximum
    -> ℝ  -- ^ resulting number
rmax r x y = if r /= 0 && abs (x-y) < r
                then y - r*sin(pi/4-asin((x-y)/r/sqrt 2)) + r
                else max x y

-- | Rounded minimum
--
-- NOTE: rmin is not associative!
rmin ::
    ℝ     -- ^ radius
    -> ℝ  -- ^ first number to round minimum
    -> ℝ  -- ^ second number to round minimum
    -> ℝ  -- ^ resulting number
rmin r x y = if r /= 0 && abs (x-y) < r
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
rmaximum _ [] = 0
rmaximum _ [a] = a
rmaximum r [a,b] = rmax r a b
rmaximum r (sortBy (flip compare) -> (a:b:_:_)) = rmax r a b
rmaximum _ _ = error "impossible."  -- (and with dependent types we could prove it!)

-- | Like rmin but on a list.
rminimum ::
    ℝ      -- ^ radius
    -> [ℝ] -- ^ numbers to take round minimum
    -> ℝ   -- ^ resulting number
rminimum _ [] = 0
rminimum _ [a] = a
rminimum r [a,b] = rmin r a b
rminimum r (sort -> (a:b:_:_)) = rmin r a b
rminimum _ _ = error "impossible."

-- | Pack the given objects in a box the given size.
pack ::
    Box2           -- ^ The box to pack within
    -> ℝ           -- ^ The space seperation between items
    -> [(Box2, a)] -- ^ Objects with their boxes
    -> ([(ℝ2, a)], [(Box2, a)] ) -- ^ Packed objects with their positions, objects that could be packed
pack (dx, dy) sep objs = packSome sortedObjs (dx, dy)
    where
        compareBoxesByY :: Box2 -> Box2 -> Ordering
        compareBoxesByY  (V2 _ ay1, V2 _ ay2)  (V2 _ by1, V2 _ by2) =
                compare (abs $ by2-by1) (abs $ ay2-ay1)

        sortedObjs = sortBy
            (\(boxa, _) (boxb, _) -> compareBoxesByY boxa boxb )
            objs

        tmap1 :: (t2 -> t) -> (t2, t1) -> (t, t1)
        tmap1 f (a,b) = (f a, b)
        tmap2 :: (t2 -> t1) -> (t, t2) -> (t, t1)
        tmap2 f (a,b) = (a, f b)

        packSome :: [(Box2,a)] -> Box2 -> ([(ℝ2,a)], [(Box2,a)])
        packSome (presObj@((V2 x1 y1,V2 x2 y2),obj):otherBoxedObjs) box@(V2 bx1 by1, V2 bx2 by2) =
            if abs (x2 - x1) <= abs (bx2-bx1) && abs (y2 - y1) <= abs (by2-by1)
            then
                let
                    row = tmap1 ((V2 (bx1-x1) (by1-y1), obj):) $
                        packSome otherBoxedObjs (V2 (bx1+x2-x1+sep) by1, V2 bx2 (by1 + y2-y1))
                    rowAndUp =
                        if abs (by2-by1) - abs (y2-y1) > sep
                        then tmap1 (fst row <> ) $
                            packSome (snd row) (V2 bx1 (by1 + y2-y1+sep), V2 bx2 by2)
                        else row
                in
                    rowAndUp
            else
                tmap2 (presObj:) $ packSome otherBoxedObjs box
        packSome [] _ = ([], [])

-- | Reflect a vector across a hyperplane defined by its normal vector.
--
-- From https://en.wikipedia.org/wiki/Reflection_(mathematics)#Reflection_through_a_hyperplane_in_n_dimensions
reflect
    :: (Num (f a), Fractional a, Metric f)
    => f a  -- ^ Mirror axis
    -> f a  -- ^ Vector to transform
    -> f a
reflect a v = v - (2 * ((v `dot` a) / (a `dot` a))) *^ a

-- | Lift a function over 'V3' into a function over 'ℝ3'.
alaV3 :: (V3 a -> V3 a) -> (a, a, a) -> (a, a, a)
alaV3 f = unpackV3 . f . packV3
{-# INLINABLE alaV3 #-}

packV3 :: (a, a, a) -> V3 a
packV3 (x, y, z) = V3 x y z
{-# INLINABLE packV3 #-}

unpackV3 :: V3 a -> (a, a, a)
unpackV3 (V3 a a2 a3) = (a, a2, a3)
{-# INLINABLE unpackV3 #-}

-- | Convert a 'Quaternion' to its constituent euler angles.
--
-- From https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Source_code_2
quaternionToEuler :: RealFloat a => Quaternion a -> (a, a, a)
quaternionToEuler (Quaternion w (V3 x y z))=
  let sinr_cosp = 2 * (w * x + y * z)
      cosr_cosp = 1 - 2 * (x * x + y * y)
      sinp = 2 * (w * y - z * x);
      siny_cosp = 2 * (w * z + x * y);
      cosy_cosp = 1 - 2 * (y * y + z * z);
      pitch = if sinp >= 1
              then signum sinp * pi / 2
              else asin sinp
      roll = atan2 sinr_cosp cosr_cosp
      yaw = atan2 siny_cosp cosy_cosp
   in (roll, pitch, yaw)

------------------------------------------------------------------------------
-- | Haskell's standard library doesn't make floating-point infinity available
-- in any convenient way, so we define it here.
infty :: (Fractional t) => t
infty = 1/0
{-# INLINABLE infty #-}
