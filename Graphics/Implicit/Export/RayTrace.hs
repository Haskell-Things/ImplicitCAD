-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.Implicit.Export.RayTrace where

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, (⋅), Obj3)
import Codec.Picture
import Control.Monad
import Data.VectorSpace
import Data.Cross hiding (normal)

-- Definitions

data Camera = Camera ℝ3 ℝ3 ℝ3 ℝ
    deriving Show

data Ray    = Ray ℝ3 ℝ3
    deriving Show

data Light  = Light ℝ3 ℝ
    deriving Show

data Scene  = Scene Obj3 Color [Light] Color

type Color  = PixelRGBA8

color :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> PixelRGBA8
color r g b a = PixelRGBA8 r g b a

dynamicImage :: Image PixelRGBA8 -> DynamicImage
dynamicImage = ImageRGBA8

-- Math

vectorDistance :: ℝ3 -> ℝ3 -> Scalar ℝ3
vectorDistance a b = magnitude (b-a)

colorMult :: RealFrac a => a -> PixelRGBA8 -> PixelRGBA8
s `colorMult` (PixelRGBA8 a b c d) = color (s `mult` a) (s `mult` b) (s `mult` c) d
    where 
        bound = max 0 . min 254
        mult x y = fromInteger . round . bound $ x * fromIntegral y

average :: [Color] -> Color
average l = 
    let    
        ((rs, gs), (bs, as)) = (\(a'',b'') -> (unzip a'', unzip b'')) $ unzip $ map
            (\(PixelRGBA8 r g b a) -> ((fromIntegral r, fromIntegral g), (fromIntegral b, fromIntegral a)))
            l :: (([ℝ], [ℝ]), ([ℝ],[ℝ]))
        n = fromIntegral $ length l :: ℝ
        (r', g', b', a') = (sum rs/n, sum gs/n, sum bs/n, sum as/n)
    in PixelRGBA8
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


intersection :: Ray -> ((ℝ,ℝ), ℝ) -> ℝ -> Obj3 -> Maybe ℝ3
intersection r@(Ray p v) ((a, aval),b) res obj =
    let
        step = 
            if      aval/(4::ℝ) > res then res
            else if aval/(2::ℝ) > res then res/(2 :: ℝ)
            else                           res/(10 :: ℝ)
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

refine' :: Int -> ℝ2 -> ℝ2 -> (ℝ -> ℝ) -> ℝ
refine' 0 (a, _) _ _ = a
refine' n (a, b) (aval, bval) obj = 
    let
        mid = (a+b)/(2::ℝ)
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

traceRay :: Ray -> ℝ -> (ℝ3, ℝ3) -> Scene -> Color
traceRay ray@(Ray cameraP cameraV) step box (Scene obj objColor lights defaultColor) =
    let
        (a,b) = rayBounds ray box
    in case intersection ray ((a, obj (cameraP ^+^ a*^cameraV)), b) step obj of
        Just p  -> flip colorMult objColor $ (sum $ [0.2] ++ do
            Light lightPos lightIntensity <- lights
            let
                ray'@(Ray _ v) = rayFromTo p lightPos
                v' = normalized v
            guard . not $ intersects ray' ((0, obj p),20) step obj
            let
                pval = obj p
                       -- FIXME: why was this here?
--                step = 0.1 :: ℝ
                dirDeriv :: ℝ3 -> ℝ
                dirDeriv v'' = (obj (p ^+^ step*^v'') ^-^ pval)/step
                deriv = (dirDeriv (1,0,0), dirDeriv (0,1,0), dirDeriv (0,0,1))
                normal = normalized $ deriv
                unitV = normalized $ v'
                proj a' b' = (a'⋅b')*^b'
                dist  = vectorDistance p lightPos
                illumination = (max 0 (normal ⋅ unitV)) * lightIntensity * (25 /dist)
                rV = 
                    let
                        normalComponent = proj v' normal
                        parComponent    = v' - normalComponent
                    in
                        normalComponent - parComponent    
            return $ illumination*(3 + 0.3*(abs $ rV ⋅ cameraV)*(abs $ rV ⋅ cameraV))
            )
        Nothing   -> defaultColor


