-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetImplicit3 (getImplicit3) where

import Prelude (id, (||), (/=), either, round, fromInteger, Either(Left, Right), abs, (-), (/), (*), sqrt, (+), atan2, max, cos, minimum, ($), sin, pi, (.), Bool(True, False), ceiling, floor, pure, (==), otherwise, (**), min, Num, Applicative)

import Graphics.Implicit.Definitions
    ( objectRounding,
      ObjectContext,
      ℕ,
      SymbolicObj3(Cube, Sphere, Cylinder, Rotate3, Transform3, Extrude,
                   ExtrudeM, ExtrudeOnEdgeOf, RotateExtrude, Shared3, Torus, Ellipsoid, BoxFrame, Link),
      Obj3,
      ℝ2,
      ℝ,
      fromℕtoℝ,
      toScaleFn,
      ℝ3 )

import Graphics.Implicit.MathUtil ( rmax, rmaximum )

import qualified Data.Either as Either (either)

-- Use getImplicit for handling extrusion of 2D shapes to 3D.
import Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared)
import Linear (V2(V2), V3(V3), _xy, _z)
import qualified Linear

import {-# SOURCE #-} Graphics.Implicit.Primitives (getImplicit)
import Control.Lens ((^.))

default (ℝ)

-- Length similar to the opengl version, needed for some of the shape definitions
openglLength :: (Linear.Metric f, Num (f ℝ), Applicative f) => f ℝ -> ℝ
openglLength v = Linear.distance (abs v) $ pure 0

-- Component wise maximum. This is what the opengl language is doing, so we need
-- it for the function as defined by the blog above.
-- See "Maximum" http://15462.courses.cs.cmu.edu/fall2019/article/20
compMax :: ℝ3 -> ℝ3 -> ℝ3
compMax (V3 a1 b1 c1) (V3 a2 b2 c2) = V3 (max a1 a2) (max b1 b2) (max c1 c2)

-- Get a function that describes the surface of the object.
getImplicit3 :: ObjectContext -> SymbolicObj3 -> Obj3
-- Primitives
getImplicit3 ctx (Cube (V3 dx dy dz)) =
    \(V3 x y z) -> rmaximum (objectRounding ctx) [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2, abs (z-dz/2) - dz/2]
getImplicit3 _ (Sphere r) =
    \(V3 x y z) -> sqrt (x*x + y*y + z*z) - r
getImplicit3 _ (Torus r1 r2) =  \(V3 x y z) -> let a = (sqrt (x**2 + y**2) - r1) in a**2 + z**2 - r2**2
getImplicit3 _ (Ellipsoid a b c) = \(V3 x y z) -> (x**2/a**2) + (y**2/b**2) + (z**2/c**2) - 1
getImplicit3 _ (Cylinder h r1 r2) = \(V3 x y z) ->
    let
        d = sqrt (x*x + y*y) - ((r2-r1)/h*z+r1)
        θ = atan2 (r2-r1) h
    in
        max (d * cos θ) (abs (z-h/2) - (h/2))
getImplicit3 _ (BoxFrame b e) = \p' ->
    let p@(V3 px py pz) = abs p' - b
        V3 qx qy qz = abs (p + pure e) - pure e
        -- Splitting out bits from https://iquilezles.org/articles/distfunctions/
        -- to make it somewhat readable.
        -- These names don't mean anything, and are just for splitting up the code.
        x', y', z' :: ℝ
        x' = openglLength (compMax (V3 px qy qz) (pure 0)) + min (max px (max qy qz)) 0
        y' = openglLength (compMax (V3 qx py qz) (pure 0)) + min (max qx (max py qz)) 0
        z' = openglLength (compMax (V3 qx qy pz) (pure 0)) + min (max qx (max qy pz)) 0
    in min (min x' y') z'
getImplicit3 _ (Link le r1 r2) = \(V3 px py pz) ->
    let q = V3 px (max (abs py - le) 0) pz
    in openglLength (V2 (openglLength (q ^. _xy) - r1) (q ^. _z)) - r2
-- Simple transforms
getImplicit3 ctx (Rotate3 q symbObj) =
    getImplicit3 ctx symbObj . Linear.rotate (Linear.conjugate q)
getImplicit3 ctx (Transform3 m symbObj) =
    getImplicit3 ctx symbObj . Linear.normalizePoint . (Linear.inv44 m Linear.!*) . Linear.point
-- 2D Based
getImplicit3 ctx (Extrude h symbObj) =
    let
        obj = getImplicit symbObj
    in
        \(V3 x y z) -> rmax (objectRounding ctx) (obj (V2 x y)) (abs (z - h/2) - h/2)
getImplicit3 ctx (ExtrudeM twist scale translate symbObj height) =
    let
        r = objectRounding ctx
        obj = getImplicit symbObj
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
getImplicit3 _ (ExtrudeOnEdgeOf symbObj1 symbObj2) =
    let
        obj1 = getImplicit symbObj1
        obj2 = getImplicit symbObj2
    in
        \(V3 x y z) -> obj1 $ V2 (obj2 (V2 x y)) z
getImplicit3 ctx (RotateExtrude totalRotation translate rotate symbObj) =
    let
        tau :: ℝ
        tau = 2 * pi
        obj = getImplicit symbObj

        is360m :: ℝ -> Bool
        is360m n = tau * fromInteger (round $ n / tau) /= n
        capped
             = is360m totalRotation
            || either ( /= pure 0) (\f -> f 0 /= f totalRotation) translate
            || either is360m (\f -> is360m (f 0 - f totalRotation)) rotate
        round' = objectRounding ctx
        translate' :: ℝ -> ℝ2
        translate' = Either.either
                (\(V2 a b) θ -> V2 (a*θ/totalRotation) (b*θ/totalRotation))
                id
                translate
        rotate' :: ℝ -> ℝ
        rotate' = Either.either
                (\t θ -> t*θ/totalRotation )
                id
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
                        [-1 .. ceiling $ (totalRotation / tau) + 1]
                    else
                        [0 .. floor $ (totalRotation - θ) / tau]
            n <- ns
            let
                θvirt = fromℕtoℝ n * tau + θ
                (V2 rshift zshift) = translate' θvirt
                twist = rotate' θvirt
                rz_pos = if twists
                        then let
                            (c,s) = (cos twist, sin twist)
                            (r',z') = (r-rshift, z-zshift)
                        in
                            V2 (c*r' - s*z') (c*z' + s*r')
                        else V2 (r - rshift) (z - zshift)
            pure $
              if capped
              then rmax round'
                    (abs (θvirt - (totalRotation / 2)) - (totalRotation / 2))
                    (obj rz_pos)
              else obj rz_pos
getImplicit3 ctx (Shared3 obj) = getImplicitShared ctx obj
