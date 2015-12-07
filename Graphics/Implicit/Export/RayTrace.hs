
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.Implicit.Export.RayTrace where

import Graphics.Implicit.ObjectUtil
import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Definitions
import Codec.Picture
import Control.Monad
import Data.VectorSpace       
import Data.AffineSpace       
import Data.Cross

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

colorMult :: RealFrac c => c -> PixelRGBA8 -> PixelRGBA8
s `colorMult` (PixelRGBA8 a b c d) = color (s `mult` a) (s `mult` b) (s `mult` c) d
    where 
        bound = max 0 . min 254
        mult x y = fromIntegral . round . bound $ x * fromIntegral y

average :: [Color] -> Color
average l = 
    let 
        ((rs, gs), (bs, as)) = (\(a',b') -> (unzip a', unzip b')) $ unzip $ map 
            (\(PixelRGBA8 r g b a) -> ((fromIntegral r, fromIntegral g), (fromIntegral b, fromIntegral a)))
            l :: (([ℝ], [ℝ]), ([ℝ],[ℝ]))
        n = fromIntegral $ length l :: ℝ
        (r, g, b, a) = (sum rs/n, sum gs/n, sum bs/n, sum as/n)
    in PixelRGBA8
        (fromIntegral . round $ r) (fromIntegral . round $ g) (fromIntegral . round $ b) (fromIntegral . round $ a)

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
                step = 0.1 :: ℝ
                dirDeriv :: ℝ3 -> ℝ
                dirDeriv v = (obj (p ^+^ step*^v) ^-^ pval)/step
                deriv = (dirDeriv (1,0,0), dirDeriv (0,1,0), dirDeriv (0,0,1))
                normal = normalized $ deriv
                unitV = normalized $ v'
                proj a b = (a⋅b)*^b
                dist  = vectorDistance p lightPos
                illumination = (max 0 (normal ⋅ unitV)) * lightIntensity * (25 /dist)
                rV = 
                    let
                        normalComponent = proj v' normal
                        parComponent    = v' - normalComponent
                    in
                        normalComponent - parComponent  
            return $ illumination*(3  + 0.3*(abs $ rV ⋅ cameraV)**2)
            )
        Nothing   -> defaultColor

instance DiscreteAproxable SymbolicObj3 DynamicImage where
    discreteAprox _ symbObj = dynamicImage $ generateImage pixelRenderer (round w) (round h)
        where
            (w,h) = (150, 150) :: ℝ2
            obj = getImplicit3 symbObj
            box@((x1,y1,z1), (_,y2,z2)) = getBox3 symbObj
            av :: ℝ -> ℝ -> ℝ
            av a b = (a+b)/(2::ℝ)
            avY = av y1 y2
            avZ = av z1 z2
            deviation = maximum [abs $ y1 - avY, abs $ y2 - avY, abs $ z1 - avZ, abs $ z2 - avZ]
            camera = Camera (x1-deviation*(2.2::ℝ), avY, avZ) (0, -1, 0) (0,0, -1) 1.0
            lights = [Light (x1-deviation*(1.5::ℝ), y1 - (0.4::ℝ)*(y2-y1), avZ) ((0.03::ℝ)*deviation) ]
            scene = Scene obj (PixelRGBA8 200 200 230 255) lights (PixelRGBA8 255 255 255 0)
            pixelRenderer :: Int -> Int -> Color
            pixelRenderer a b = renderScreen 
                ((fromIntegral a :: ℝ)/w - (0.5::ℝ)) ((fromIntegral b :: ℝ)/h - (0.5 ::ℝ))
            renderScreen :: ℝ -> ℝ -> Color
            renderScreen a b =
                    average $ [
                        traceRay 
                            (cameraRay camera ((a,b) ^+^ ( 0.25/w, 0.25/h)))
                            2 box scene,
                        traceRay 
                            (cameraRay camera ((a,b) ^+^ (-0.25/w, 0.25/h)))
                            0.5 box scene,
                        traceRay 
                            (cameraRay camera ((a,b) ^+^ (0.25/w, -0.25/h)))
                            0.5 box scene,
                        traceRay 
                            (cameraRay camera ((a,b) ^+^ (-0.25/w,-0.25/h)))
                            0.5 box scene
                        ]


instance DiscreteAproxable SymbolicObj2 DynamicImage where
    discreteAprox _ symbObj = dynamicImage $ generateImage pixelRenderer (round w) (round h)
        where
            (w,h) = (150, 150) :: ℝ2
            obj = getImplicit2 symbObj
            (p1@(x1,_), p2@(_,y2)) = getBox2 symbObj
            (dx, dy) = p2 ^-^ p1
            dxy = max dx dy
            pixelRenderer :: Int -> Int -> Color
            pixelRenderer a b = color
                where
                    xy a b = ((x1,y2) .-^ (dxy-dx, dy-dxy)^/2) .+^ dxy*^(a/w, -b/h)
                    s = 0.25 :: ℝ
                    (a', b') = (realToFrac a, realToFrac b)
                    color = average [objColor $ xy a' b', objColor $ xy a' b',
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s),
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s)]
            objColor p = if obj p < 0 then PixelRGBA8 150 150 160 255 else PixelRGBA8 255 255 255 0


