-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetImplicit3 (getImplicit3) where

import Prelude (const, Fractional, Either(Left, Right), abs, (-), (/), (*), sqrt, (+), atan2, max, cos, fmap, minimum, ($), (**), sin, pi, (.), Bool(True, False), ceiling, floor, pure, error, (>), (&&), (<), (==), otherwise, (<$>))

import Graphics.Implicit.Definitions (ℝ, ℕ, ℝ2, ℝ3, (⋯/), Obj3,
                                      SymbolicObj3(Empty3, Full3, Shell3, UnionR3, IntersectR3, DifferenceR3, Translate3, Scale3, Rotate3,
                                                   Outset3, CubeR, Sphere, Cylinder, Complement3, EmbedBoxedObj3, Mirror3,
                                                   ExtrudeR, ExtrudeRM, ExtrudeOnEdgeOf, RotateExtrude, ExtrudeRotateR), fromℕtoℝ, toScaleFn, minℝ)

import Graphics.Implicit.MathUtil (alaV3, reflect, rmaximum, rminimum, rmax)

import Data.Maybe (fromMaybe, isJust)
import qualified Linear as Q

import qualified Data.Either as Either (either)

-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import  Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2)
import Data.VectorSpace (AdditiveGroup((^-^)))

default (ℝ)

infty :: Fractional t => t
infty = 1 / 0

-- Get a function that describes the surface of the object.
getImplicit3 :: SymbolicObj3 -> Obj3
-- Primitives
getImplicit3 Empty3 = const infty
getImplicit3 Full3 = const $ -infty
getImplicit3 (CubeR r (dx, dy, dz)) =
    \(x,y,z) -> rmaximum r [abs (x-dx/2) - dx/2, abs (y-dy/2) - dy/2, abs (z-dz/2) - dz/2]
getImplicit3 (Sphere r) =
    \(x,y,z) -> sqrt (x*x + y*y + z*z) - r
getImplicit3 (Cylinder h r1 r2) = \(x,y,z) ->
    let
        d = sqrt (x*x + y*y) - ((r2-r1)/h*z+r1)
        θ = atan2 (r2-r1) h
    in
        max (d * cos θ) (abs (z-h/2) - (h/2))
-- (Rounded) CSG
getImplicit3 (Complement3 symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> - obj p
getImplicit3 (UnionR3 _ []) = getImplicit3 Empty3
-- TODO(sandy): This is non-associative somehow. Fix it.
-- Test case:
--   UnionR3 0 (obj : [ UnionR3 0 objs ]) =~=
--     UnionR3 0 (obj : objs)
--
-- Result:
--   Falsified (after 82 tests and 51 shrinks):
--     Scale3 (2.0,3.0,5.0) (Sphere 34.21)
--     [ExtrudeR 0.0 (Shell2 17.56485601360011 (UnionR2 2.0027171946836613 [Shell2 1.5984749452135119 (Scale2 (1.0898601265130337,-0.29055538926420654) (PolygonR 0.23223027098261984 [(0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0)
--,  .0),(0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0)]))])) 1.0,CubeR 0.0 (10.0,1.0,1.0)]
--     ((0.0,0.0,0.0),())
--     Inside /= Surface
getImplicit3 (UnionR3 r symbObjs) =
  \p -> rminimum r $ fmap ($p) $ getImplicit3 <$> symbObjs

getImplicit3 (IntersectR3 r symbObjs) =
  \p -> rmaximum r $ fmap ($p) $ getImplicit3 <$> symbObjs

getImplicit3 (DifferenceR3 r symbObj symbObjs) =
    let
        tailObjs = getImplicit3 <$> symbObjs
        headObj = getImplicit3 symbObj
        complement :: Obj3 -> ℝ3 -> ℝ
        complement obj' p = - obj' p
    in
      \p -> do
        let
          maxTail = rmaximum r $ fmap ($p) $ complement <$> tailObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicit3 (Translate3 v symbObj) =
    let
        obj = getImplicit3 symbObj
    in
        \p -> obj (p ^-^ v)
getImplicit3 (Scale3 s@(sx,sy,sz) symbObj) =
    let
        obj = getImplicit3 symbObj
        k = abs (sx*sy*sz) ** (1/3)
    in
        \p -> k * obj (p ⋯/ s)
getImplicit3 (Rotate3 q symbObj) =
    getImplicit3 symbObj . alaV3 (Q.rotate $ Q.conjugate q)
getImplicit3 (Mirror3 v symbObj) =
    getImplicit3 symbObj . reflect v
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
        height' (x,y) = case height of
            Left n -> n
            Right f -> f (x,y)
        -- FIXME: twist functions should have access to height, if height is allowed to vary.
        twistVal :: Either ℝ (ℝ -> ℝ) -> ℝ -> ℝ -> ℝ
        twistVal twist' z h =
          case twist' of
                   Left twval  -> if twval == 0
                                  then 0
                                  else twval * (z / h)
                   Right twfun -> twfun z
        translatePos :: Either ℝ2 (ℝ -> ℝ2) -> ℝ -> ℝ2 -> ℝ2
        translatePos trans z (x, y) = (x - xTrans, y - yTrans)
          where
            (xTrans, yTrans) = case trans of
                                 Left  tval -> tval
                                 Right tfun -> tfun z
        scaleVec :: ℝ -> ℝ2 -> ℝ2
        scaleVec z (x, y) = let (sx, sy) = toScaleFn scale z
                            in  (x / sx, y / sy)
        rotateVec :: ℝ -> ℝ2 -> ℝ2
        rotateVec θ (x,y)
          | θ == 0    = (x,y)
          | otherwise = (x*cos θ + y*sin θ, y*cos θ - x*sin θ)
        k :: ℝ
        k = pi/180
    in
        \(x,y,z) ->
          let
            h = height' (x,y)
            res = rmax r
                (obj
                 . rotateVec (-k*twistVal twist z h)
                 . scaleVec z
                 . translatePos translate z
                 $ (x,y))
                (abs (z - h/2) - h/2)
          in
            res
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
        capped = isJust round
        round' = fromMaybe 0 round
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
                (rshift, zshift) = translate' θvirt
                twist = rotate' θvirt
                rz_pos = if twists
                        then let
                            (c,s) = (cos (twist*k), sin (twist*k))
                            (r',z') = (r-rshift, z-zshift)
                        in
                            (c*r' - s*z', c*z' + s*r')
                        else (r - rshift, z - zshift)
            pure $
              if capped
              then rmax round'
                    (abs (θvirt - (totalRotation' / 2)) - (totalRotation' / 2))
                    (obj rz_pos)
              else obj rz_pos

-- FIXME: implement this, or implement a fallthrough function.
--getImplicit3 (ExtrudeRotateR) =
getImplicit3 ExtrudeRotateR{} = error "ExtrudeRotateR unimplimented!"
