-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow our DiscreteAproxable class to handle multiple parameters.
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Implicit.Export.DiscreteAproxable where

import Graphics.Implicit.Definitions

import Graphics.Implicit.ObjectUtil

import Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh)
import Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour)
import Graphics.Implicit.Export.Util (normTriangle)    
    
import Graphics.Implicit.Export.RayTrace (dynamicImage, Color, average, Camera(..), Light(..), Scene(..), traceRay, cameraRay)

import Codec.Picture

import Data.VectorSpace
import Data.AffineSpace


-- | There is a discrete way to aproximate this object.
--   eg. Aproximating a 3D object with a tirangle mesh
--       would be DiscreteApproxable Obj3 TriangleMesh
class DiscreteAproxable obj aprox where
    discreteAprox :: ℝ -> obj -> aprox

instance DiscreteAproxable SymbolicObj3 TriangleMesh where
    discreteAprox res obj = symbolicGetMesh res obj

instance DiscreteAproxable SymbolicObj3 NormedTriangleMesh where
    discreteAprox res obj = map (normTriangle res (getImplicit3 obj)) $ symbolicGetMesh res obj

-- FIXME: magic numbers.
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

instance DiscreteAproxable SymbolicObj2 [Polyline] where
    discreteAprox res obj = symbolicGetContour res obj

instance DiscreteAproxable SymbolicObj2 DynamicImage where
    discreteAprox _ symbObj = dynamicImage $ generateImage pixelRenderer (round w) (round h)
        where
            (w,h) = (150, 150) :: ℝ2
            obj = getImplicit2 symbObj
            (p1@(x1,_), p2@(_,y2)) = getBox2 symbObj
            (dx, dy) = p2 ^-^ p1
            dxy = max dx dy
            pixelRenderer :: Int -> Int -> Color
            pixelRenderer mya myb = mycolor
                where
                    xy a b = ((x1,y2) .-^ (dxy-dx, dy-dxy)^/2) .+^ dxy*^(a/w, -b/h)
                    s = 0.25 :: ℝ
                    (a', b') = (realToFrac mya, realToFrac myb)
                    mycolor = average [objColor $ xy a' b', objColor $ xy a' b',
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s),
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s)]
            objColor p = if obj p < 0 then PixelRGBA8 150 150 160 255 else PixelRGBA8 255 255 255 0




                            
