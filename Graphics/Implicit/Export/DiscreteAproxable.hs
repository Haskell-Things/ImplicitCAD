-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow our DiscreteAproxable class to handle multiple parameters.
{-# LANGUAGE MultiParamTypeClasses #-}

-- For the instance declaration of DiscreteAproxable SymbolicObj2 [Polyline]
{-# LANGUAGE FlexibleInstances #-}

-- | A module for retrieving approximate represententations of objects.
module Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable, discreteAprox) where

import Prelude((-), (/), ($), (<), map, round, (+), maximum, abs, (*), fromIntegral, max, realToFrac, Int)

-- Definitions for our number system, objects, and the things we can use to approximately represent objects.
import Graphics.Implicit.Definitions (ℝ, ℝ2, SymbolicObj2, SymbolicObj3, Polyline, Triangle, TriangleMesh(TriangleMesh), NormedTriangleMesh(NormedTriangleMesh))

import Graphics.Implicit.ObjectUtil (getImplicit2, getImplicit3, getBox2, getBox3)

import Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh)

import Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour)

import Graphics.Implicit.Export.Util (normTriangle)

-- We are the only ones that use this.
import Graphics.Implicit.Export.RayTrace (Color(Color), Camera(Camera), Light(Light), Scene(Scene), average, traceRay, cameraRay)

import Codec.Picture (DynamicImage(ImageRGBA8), PixelRGBA8(PixelRGBA8), generateImage)

import Data.VectorSpace ((^+^), (^/), (*^), (^-^))

import Data.AffineSpace ((.-^), (.+^))

default (ℝ)

unmesh :: TriangleMesh -> [Triangle]
unmesh (TriangleMesh m) = m

-- | There is a discrete way to aproximate this object.
--   eg. Aproximating a 3D object with a triangle mesh
--       would be DiscreteApproxable Obj3 TriangleMesh
class DiscreteAproxable obj aprox where
    discreteAprox :: ℝ -> obj -> aprox

instance DiscreteAproxable SymbolicObj3 TriangleMesh where
    discreteAprox = symbolicGetMesh

instance DiscreteAproxable SymbolicObj3 NormedTriangleMesh where
    discreteAprox res obj = NormedTriangleMesh $ map (normTriangle res (getImplicit3 obj)) $ unmesh $ symbolicGetMesh res obj

-- FIXME: way too many magic numbers.
-- FIXME: adjustable resolution!
instance DiscreteAproxable SymbolicObj3 DynamicImage where
    discreteAprox _ symbObj = ImageRGBA8 $ generateImage pixelRenderer (round w) (round h)
        where
            -- | Size of the image to produce.
            (w,h) = (150, 150) :: ℝ2
            obj = getImplicit3 symbObj
            box@((x1,y1,z1), (_,y2,z2)) = getBox3 symbObj
            av :: ℝ -> ℝ -> ℝ
            av a b = (a+b)/2
            avY = av y1 y2
            avZ = av z1 z2
            deviation = maximum [abs $ y1 - avY, abs $ y2 - avY, abs $ z1 - avZ, abs $ z2 - avZ]
            camera = Camera (x1-deviation*2.2, avY, avZ) (0, -1, 0) (0,0, -1) 1.0
            lights = [Light (x1-deviation*1.5, y1 - 0.4*(y2-y1), avZ) (0.03*deviation) ]
            scene = Scene obj (Color 200 200 230 255) lights (Color 255 255 255 0)
            -- | passed to generateImage, it's external, and determines this type.
            pixelRenderer :: Int -> Int -> PixelRGBA8
            pixelRenderer a b = renderScreen
                (fromIntegral a/w - 0.5) (fromIntegral b/h - 0.5)
            renderScreen :: ℝ -> ℝ -> PixelRGBA8
            renderScreen a b =
                colorToPixelRGBA8 $
                    average [
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
                    where
                      colorToPixelRGBA8 :: Color -> PixelRGBA8
                      colorToPixelRGBA8 (Color rr gg bb aa) = PixelRGBA8 rr gg bb aa

instance DiscreteAproxable SymbolicObj2 [Polyline] where
    discreteAprox = symbolicGetContour

-- FIXME: way too many magic numbers.
-- FIXME: adjustable resolution?
instance DiscreteAproxable SymbolicObj2 DynamicImage where
    discreteAprox _ symbObj = ImageRGBA8 $ generateImage pixelRenderer (round w) (round h)
        where
            -- | Size of the image to produce.
            (w,h) = (150, 150) :: ℝ2
            obj = getImplicit2 symbObj
            (p1@(x1,_), p2@(_,y2)) = getBox2 symbObj
            (dx, dy) = p2 ^-^ p1
            dxy = max dx dy
            -- | passed to generateImage, it's external, and determines this type.
            pixelRenderer :: Int -> Int -> PixelRGBA8
            pixelRenderer mya myb = mycolor
                where
                    xy a b = ((x1,y2) .-^ (dxy-dx, dy-dxy)^/2) .+^ dxy*^(a/w, -b/h)
                    s = 0.25 :: ℝ
                    (a', b') = (realToFrac mya, realToFrac myb) :: ℝ2
                    mycolor = colorToPixelRGBA8 $ average [objColor $ xy a' b', objColor $ xy a' b',
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s),
                        objColor $ xy (a'+s) (b'+s),
                        objColor $ xy (a'-s) (b'-s)]
                    colorToPixelRGBA8 :: Color -> PixelRGBA8
                    colorToPixelRGBA8 (Color rr gg bb aa) = PixelRGBA8 rr gg bb aa
            objColor p = if obj p < 0 then Color 150 150 160 255 else Color 255 255 255 0

