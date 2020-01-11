-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.RayTrace( Color(Color), average, Camera(Camera), Light(Light), Scene(Scene), traceRay, cameraRay) where

import Prelude(Show, RealFrac, Maybe(Just, Nothing), Bool(False, True), (-), (.), ($), (*), (/), min, fromInteger, max, round, fromIntegral, unzip, fmap, length, sum, maximum, minimum, (>), (+), (<), (==), pred, flip, not, abs, floor, toRational, otherwise, pure)

-- Our number system, and the definition of a 3D object.
import Graphics.Implicit.Definitions (ℝ, Fastℕ, ℝ2, ℝ3, (⋅), Obj3)

import Codec.Picture (Pixel8)

import Control.Monad (guard)

import Control.Arrow ((***))

import Data.VectorSpace (Scalar, magnitude, (^+^), (*^), normalized, (^-^), InnerSpace)

import Data.Cross (cross3)

default (Fastℕ, ℝ)

-- Definitions

data Camera = Camera ℝ3 ℝ3 ℝ3 ℝ
    deriving Show

-- | A ray. A point, and a normal pointing in the direction the ray is going.
data Ray    = Ray ℝ3 ℝ3
    deriving Show

data Scene  = Scene Obj3 Color [Light] Color

-- | A light source. source point, and intensity.
data Light  = Light ℝ3 ℝ
    deriving Show

-- | A colour. Red Green Blue and Alpha components.
data Color  = Color Pixel8 Pixel8 Pixel8 Pixel8

-- Math

-- | The distance traveled by a line segment from the first point to the second point.
vectorDistance :: ℝ3 -> ℝ3 -> Scalar ℝ3
vectorDistance a b = magnitude (b-a)

-- | Multiply a colour by an intensity.
colorMult :: Pixel8 -> Color -> Color
s `colorMult` (Color a b c d) = Color (s `mult` a) (s `mult` b) (s `mult` c) d
    where
        bound :: RealFrac a => a -> a
        bound = max 0 . min 255
        mult :: Pixel8 -> Pixel8 -> Pixel8
        mult x y = round . bound . toRational $ x * y

-- | Average a set of colours.
average :: [Color] -> Color
average l =
    let
        ((rs, gs), (bs, as)) = (unzip *** unzip) . unzip $ fmap
            (\(Color r g b a) -> ((fromIntegral r, fromIntegral g), (fromIntegral b, fromIntegral a)))
            l :: (([ℝ], [ℝ]), ([ℝ], [ℝ]))
        n :: ℝ
        n = fromIntegral $ length l
        (r', g', b', a') = (sum rs/n, sum gs/n, sum bs/n, sum as/n)
    in Color
        (fromInteger . round $ r') (fromInteger . round $ g') (fromInteger . round $ b') (fromInteger . round $ a')

-- Ray Utilities

cameraRay :: Camera -> ℝ2 -> Ray
cameraRay (Camera p vx vy f) (x,y) =
    let
        v  = vx `cross3` vy
        p' = p ^+^ f*^v ^+^ x*^vx ^+^ y*^vy
        n  = normalized (p' ^-^ p)
    in
        Ray p' n

-- | Create a ray from two points.
rayFromTo :: ℝ3 -> ℝ3 -> Ray
rayFromTo p1 p2 = Ray p1 (normalized $ p2 ^-^ p1)

rayBounds :: Ray -> (ℝ3, ℝ3) -> ℝ2
rayBounds ray box =
    let
        Ray (cPx, cPy, cPz) (cVx, cVy, cVz) = ray
        ((x1,y1,z1),(x2,y2,z2)) = box
        xbounds = [(x1 - cPx)/cVx, (x2-cPx)/cVx]
        ybounds = [(y1-cPy)/cVy, (y2-cPy)/cVy]
        zbounds = [(z1-cPz)/cVz, (z2-cPz)/cVz]
        lower   = maximum [minimum xbounds, minimum ybounds, minimum zbounds]
        upper   = minimum [maximum xbounds, maximum ybounds, maximum zbounds]
    in
        (lower, upper)

-- Intersection
-- FIXME: magic numbers.
intersection :: Ray -> ((ℝ,ℝ), ℝ) -> ℝ -> Obj3 -> Maybe ℝ3
intersection r@(Ray p v) ((a, aval),b) res obj =
    let
        step | aval/4 > res = res
             | aval/2 > res = res/2
             | otherwise = res/10
        a'  = a + step
        a'val = obj (p ^+^ a'*^v)
    in if a'val < 0
    then
        let a'' = refine (a,a') (\s -> obj (p ^+^ s*^v))
        in Just (p ^+^ a''*^v)
    else if a' < b
    then intersection r ((a',a'val), b) res obj
    else Nothing

refine :: ℝ2 -> (ℝ -> ℝ) -> ℝ
refine (a, b) obj =
    let
        (aval, bval) = (obj a, obj b)
    in if bval < aval
    then refine' 10 (a, b) (aval, bval) obj
    else refine' 10 (b, a) (aval, bval) obj

refine' :: Fastℕ -> ℝ2 -> ℝ2 -> (ℝ -> ℝ) -> ℝ
refine' 0 (a, _) _ _ = a
refine' n (a, b) (aval, bval) obj =
    let
        mid = (a+b)/2
        midval = obj mid
    in
        if midval == 0
        then mid
        else if midval < 0
        then refine' (pred n) (a, mid) (aval, midval) obj
        else refine' (pred n) (mid, b) (midval, bval) obj

intersects :: Ray -> ((ℝ, ℝ), ℝ) -> ℝ -> Obj3 -> Bool
intersects a b c d = case intersection a b c d of
    Nothing -> False
    Just _  -> True

-- Trace
-- FIXME: magic numbers.
traceRay :: Ray -> ℝ -> (ℝ3, ℝ3) -> Scene -> Color
traceRay ray@(Ray cameraP cameraV) step box (Scene obj objColor lights defaultColor) =
    let
        (a,b) = rayBounds ray box
    in case intersection ray ((a, obj (cameraP ^+^ a*^cameraV)), b) step obj of
        Just p  -> flip colorMult objColor $ floor (sum $ 0.2 : do
            Light lightPos lightIntensity <- lights
            let
                ray'@(Ray _ v) = rayFromTo p lightPos
                v' = normalized v
            guard . not $ intersects ray' ((0, obj p),20) step obj
            let
                pval = obj p
                dirDeriv :: ℝ3 -> ℝ
                dirDeriv v'' = (obj (p ^+^ step*^v'') ^-^ pval)/step
                deriv = (dirDeriv (1,0,0), dirDeriv (0,1,0), dirDeriv (0,0,1))
                normal = normalized deriv
                unitV = normalized v'
                proj :: InnerSpace v => v -> v -> v
                proj a' b' = (a'⋅b')*^b'
                dist  = vectorDistance p lightPos
                illumination = max 0 (normal ⋅ unitV) * lightIntensity * (25 /dist)
                rV =
                    let
                        normalComponent = proj v' normal
                        parComponent    = v' - normalComponent
                    in
                        normalComponent - parComponent
            pure $ illumination*(3 + 0.3*abs(rV ⋅ cameraV)*abs(rV ⋅ cameraV))
            )
        Nothing   -> defaultColor


