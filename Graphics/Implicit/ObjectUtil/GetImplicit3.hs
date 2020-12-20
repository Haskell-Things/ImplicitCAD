-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetImplicit3 (getImplicit3) where

import Prelude (Either(Left, Right), abs, (-), (/), (*), sqrt, (+), atan2, max, cos, minimum, ($), sin, pi, (.), Bool(True, False), ceiling, floor, pure, (==), otherwise)

import Graphics.Implicit.Definitions
    ( ℕ, SymbolicObj3(CubeR, Sphere, Cylinder, Rotate3, ExtrudeR, ExtrudeRM, ExtrudeOnEdgeOf, RotateExtrude, Shared3), Obj3, ℝ2, ℝ, fromℕtoℝ, toScaleFn )

import Graphics.Implicit.MathUtil ( rmax, rmaximum )

import Data.Maybe (fromMaybe, isJust)
import qualified Linear as Q

import qualified Data.Either as Either (either)

-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import  Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2)
import Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared)
import Linear (V2(V2), V3(V3))

default (ℝ)

-- Get a function that describes the surface of the object.
getImplicit3 :: SymbolicObj3 -> Obj3
-- Primitives
getImplicit3 (CubeR r (V3 dx dy dz)) =
    \(V3 x y z) -> rmaximum r [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2, abs (z-dz/2) - dz/2]
getImplicit3 (Sphere r) =
    \(V3 x y z) -> sqrt (x*x + y*y + z*z) - r
getImplicit3 (Cylinder h r1 r2) = \(V3 x y z) ->
    let
        d = sqrt (x*x + y*y) - ((r2-r1)/h*z+r1)
        θ = atan2 (r2-r1) h
    in
        max (d * cos θ) (abs (z-h/2) - (h/2))
-- Simple transforms
getImplicit3 (Rotate3 q symbObj) =
    getImplicit3 symbObj . Q.rotate (Q.conjugate q)

-- 2D Based
getImplicit3 (ExtrudeR r symbObj h) =
    let
        obj = getImplicit2 symbObj
    in
        \(V3 x y z) -> rmax r (obj (V2 x y)) (abs (z - h/2) - h/2)
getImplicit3 (ExtrudeRM r twist scale translate symbObj height) =
    let
        obj = getImplicit2 symbObj
        height' (V2 x y) = case height of
            Left n -> n
            Right f -> f (V2 x y)
        -- FIXME: twist functions should have access to height, if height is allowed to vary.
        twistVal :: Either ℝ (ℝ -> ℝ) -> ℝ -> ℝ -> ℝ
        twistVal twist' z h =
          case twist' of
                   Left twval  -> if twval == 0
                                  then 0
                                  else twval * (z / h)
                   Right twfun -> twfun z
        translatePos :: Either ℝ2 (ℝ -> ℝ2) -> ℝ -> ℝ2 -> ℝ2
        translatePos trans z (V2 x y) = V2 (x - xTrans) (y - yTrans)
          where
            (V2 xTrans yTrans) = case trans of
                                 Left  tval -> tval
                                 Right tfun -> tfun z
        scaleVec :: ℝ -> ℝ2 -> ℝ2
        scaleVec z (V2 x y) = let (V2 sx sy) = toScaleFn scale z
                               in V2 (x / sx) (y / sy)
        rotateVec :: ℝ -> ℝ2 -> ℝ2
        rotateVec θ (V2 x y)
          | θ == 0    = V2 x y
          | otherwise = V2 (x*cos θ + y*sin θ) (y*cos θ - x*sin θ)
        k :: ℝ
        k = pi/180
    in
        \(V3 x y z) ->
          let
            h = height' $ V2 x y
            res = rmax r
                (obj
                 . rotateVec (-k*twistVal twist z h)
                 . scaleVec z
                 . translatePos translate z
                 $ V2 x y )
                (abs (z - h/2) - h/2)
          in
            res
getImplicit3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
    let
        obj1 = getImplicit2 symbObj1
        obj2 = getImplicit2 symbObj2
    in
        \(V3 x y z) -> obj1 $ V2 (obj2 (V2 x y)) z
getImplicit3 (RotateExtrude totalRotation round translate rotate symbObj) =
    let
        tau :: ℝ
        tau = 2 * pi
        k :: ℝ
        k   = tau / 360
        totalRotation' = totalRotation*k
        obj = getImplicit2 symbObj
        capped = isJust round
        round' = fromMaybe 0 round
        translate' :: ℝ -> ℝ2
        translate' = Either.either
                (\(V2 a b) θ -> V2 (a*θ/totalRotation') (b*θ/totalRotation'))
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
        \(V3 x y z) -> minimum $ do
            let
                r = sqrt $ x*x + y*y
                θ = atan2 y x
                ns :: [ℕ]
                ns =
                    if capped
                    then -- we will cap a different way, but want leeway to keep the function cont
                        [-1 .. ceiling $ (totalRotation' / tau) + 1]
                    else
                        [0 .. floor $ (totalRotation' - θ) / tau]
            n <- ns
            let
                θvirt = fromℕtoℝ n * tau + θ
                (V2 rshift zshift) = translate' θvirt
                twist = rotate' θvirt
                rz_pos = if twists
                        then let
                            (c,s) = (cos (twist*k), sin (twist*k))
                            (r',z') = (r-rshift, z-zshift)
                        in
                            V2 (c*r' - s*z') (c*z' + s*r')
                        else V2 (r - rshift) (z - zshift)
            pure $
              if capped
              then rmax round'
                    (abs (θvirt - (totalRotation' / 2)) - (totalRotation' / 2))
                    (obj rz_pos)
              else obj rz_pos
getImplicit3 (Shared3 obj) = getImplicitShared obj

