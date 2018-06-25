-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

module Graphics.Implicit.ObjectUtil.GetImplicit3 (getImplicit3) where

import Prelude (Either(Left, Right), abs, (-), (/), (*), sqrt, (+), atan2, max, cos, map, (==), minimum, ($), maximum, (**), sin, const, pi, (.), Bool(True, False), ceiling, floor, fromIntegral, return, error, head, tail, Num)

import Graphics.Implicit.Definitions (ℝ, Fastℕ, ℝ2, ℝ3, (⋯/), Obj3,
                                      SymbolicObj3(Shell3, UnionR3, IntersectR3, DifferenceR3, Translate3, Scale3, Rotate3,
                                                   Outset3, Rect3R, Sphere, Cylinder, Complement3, EmbedBoxedObj3, Rotate3V,
                                                   ExtrudeR, ExtrudeRM, ExtrudeOnEdgeOf, RotateExtrude, ExtrudeRotateR))
import Graphics.Implicit.MathUtil (rmaximum, rminimum, rmax)
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Data.VectorSpace ((^-^), (^+^), (^*), (<.>), normalized)

-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import  Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2)

getImplicit3 :: SymbolicObj3 -> Obj3
-- Primitives
getImplicit3 (Rect3R r (x1,y1,z1) (x2,y2,z2)) =
    \(x,y,z) -> let (dx, dy, dz) = (x2-x1, y2-y1, z2-z1)
                in
                  rmaximum r [abs (x-dx/2-x1) - dx/2, abs (y-dy/2-y1) - dy/2, abs (z-dz/2-z1) - dz/2]
getImplicit3 (Sphere r ) =
    \(x,y,z) -> sqrt (x*x + y*y + z*z) - r
getImplicit3 (Cylinder h r1 r2) = \(x,y,z) ->
    let
        d = sqrt(x*x + y*y) - ((r2-r1)/h*z+r1)
        θ = atan2 (r2-r1) h
    in
        max (d * cos θ) (abs(z-h/(2::ℝ)) - h/(2::ℝ))
-- (Rounded) CSG
getImplicit3 (Complement3 symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> - obj p
getImplicit3 (UnionR3 r symbObjs) =
    let
        objs = map getImplicit3 symbObjs
    in
        if r == 0
        then \p -> minimum $ map ($p) objs
        else \p -> rminimum r $ map ($p) objs
getImplicit3 (IntersectR3 r symbObjs) =
    let
        objs = map getImplicit3 symbObjs
    in
        if r == 0
        then \p -> maximum $ map ($p) objs
        else \p -> rmaximum r $ map ($p) objs
getImplicit3 (DifferenceR3 r symbObjs) =
    let
        objs = map getImplicit3 symbObjs
        obj = head objs
        complement :: forall a t. Num a => (t -> a) -> t -> a
        complement obj' p = - obj' p
    in
        if r == 0
        then \p -> maximum $ map ($p) $ obj:map complement (tail objs)
        else \p -> rmaximum r $ map ($p) $ obj:map complement (tail objs)
-- Simple transforms
getImplicit3 (Translate3 v symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> obj (p ^-^ v)
getImplicit3 (Scale3 s@(sx,sy,sz) symbObj) =
    let
        obj = getImplicit3 symbObj
        k = abs(sx*sy*sz)**(1/3)
    in
        \p -> k * obj (p ⋯/ s)
getImplicit3 (Rotate3 (yz, zx, xy) symbObj) =
    let
        obj = getImplicit3 symbObj
        rotateYZ :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
        rotateYZ θ obj' (x,y,z) = obj' ( x, y*cos θ + z*sin θ, z*cos θ - y*sin θ)
        rotateZX :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
        rotateZX θ obj' (x,y,z) = obj' ( x*cos θ - z*sin θ, y, z*cos θ + x*sin θ)
        rotateXY :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
        rotateXY θ obj' (x,y,z) = obj' ( x*cos θ + y*sin θ, y*cos θ - x*sin θ, z)
    in
        rotateXY xy $ rotateZX zx  $ rotateYZ yz obj
getImplicit3 (Rotate3V θ axis symbObj) =
    let
        axis' = normalized axis
        obj = getImplicit3 symbObj
        -- Note: this is ripped from data.cross.
        cross3 :: forall t. Num t => (t, t, t) -> (t, t, t) -> (t, t, t)
        cross3 (ax,ay,az) (bx,by,bz) = ( ay * bz - az * by
                                       , az * bx - ax * bz
                                       , ax * by - ay * bx )
    in
        \v -> obj $
            v ^* cos θ
            ^-^ (axis' `cross3` v) ^* sin θ
            ^+^ (axis' ^* (axis' <.> (v ^* (1 - cos θ))))
-- Boundary mods
getImplicit3 (Shell3 w symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> abs (obj p) - w/2
getImplicit3 (Outset3 d symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> obj p - d
-- Misc
getImplicit3 (EmbedBoxedObj3 (obj,_)) = obj
-- 2D Based
getImplicit3 (ExtrudeR r symbObj h) =
    let
        obj = getImplicit2 symbObj
    in
        \(x,y,z) -> rmax r (obj (x,y)) (abs (z - h/2) - h/2)
getImplicit3 (ExtrudeRM r twist scale translate symbObj height) =
    let
        obj = getImplicit2 symbObj
        twist' = Maybe.fromMaybe (const 0) twist
        scale' = Maybe.fromMaybe (const 1) scale
        translate' = Maybe.fromMaybe (const (0,0)) translate
        height' (x,y) = case height of
            Left n -> n
            Right f -> f (x,y)
        scaleVec :: ℝ -> ℝ2 -> ℝ2
        scaleVec  s (x,y) = (x/s, y/s)
        rotateVec :: ℝ -> ℝ2 -> ℝ2
        rotateVec θ (x,y) = (x*cos θ + y*sin θ, y*cos θ - x*sin θ)
        k = (pi :: ℝ)/(180:: ℝ)
    in
        \(x,y,z) -> let h = height' (x,y) in
            rmax r
                (obj . rotateVec (-k*twist' z) . scaleVec (scale' z) . (\a -> a ^-^ translate' z) $ (x,y))
                (abs (z - h/2) - h/2)
getImplicit3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
    let
        obj1 = getImplicit2 symbObj1
        obj2 = getImplicit2 symbObj2
    in
        \(x,y,z) -> obj1 (obj2 (x,y), z)
getImplicit3 (RotateExtrude totalRotation round translate rotate symbObj) =
    let
        tau :: ℝ
        tau = 2 * pi
        k :: ℝ
        k   = tau / 360
        totalRotation' = totalRotation*k
        obj = getImplicit2 symbObj
        capped = Maybe.isJust round
        round' = Maybe.fromMaybe 0 round
        translate' :: ℝ -> ℝ2
        translate' = Either.either
                (\(a,b) θ -> (a*θ/totalRotation', b*θ/totalRotation'))
                (. (/k))
                translate
        rotate' :: ℝ -> ℝ
        rotate' = Either.either
                (\t θ -> t*θ/totalRotation' )
                (. (/k))
                rotate
        twists = case rotate of
                   Left 0  -> True
                   _       -> False
    in
        \(x,y,z) -> minimum $ do
            
            let
                r = sqrt (x*x + y*y)
                θ = atan2 y x
                ns :: [Fastℕ]
                ns =
                    if capped
                    then -- we will cap a different way, but want leeway to keep the function cont
                        [-1 .. ceiling (totalRotation' / tau) + 1]
                    else
                        [0 .. floor $ (totalRotation' - θ) / tau]
            n <- ns
            let
                θvirt = fromIntegral n * tau + θ
                (rshift, zshift) = translate' θvirt
                twist = rotate' θvirt
                rz_pos = if twists
                        then let
                            (c,s) = (cos(twist*k), sin(twist*k))
                            (r',z') = (r-rshift, z-zshift)
                        in
                            (c*r' - s*z', c*z' + s*r')
                        else (r - rshift, z - zshift)
            return $
                if capped
                then rmax round'
                    (abs (θvirt - (totalRotation' / 2)) - (totalRotation' / 2))
                    (obj rz_pos)
                else obj rz_pos
-- FIXME: implement this, or implement a fallthrough function.
--getImplicit3 (ExtrudeRotateR) =
getImplicit3 ExtrudeRotateR{} = error "ExtrudeRotateR unimplimented!"
