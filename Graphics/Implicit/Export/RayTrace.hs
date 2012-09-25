
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Graphics.Implicit.Export.RayTrace where

import Prelude hiding ((+),(-),(*),(/))
import qualified Prelude as P
import Graphics.Implicit.ObjectUtil
import Graphics.Implicit.Definitions
import Graphics.Implicit.SaneOperators
import Graphics.Implicit.Export.Definitions
import Codec.Picture
import Control.Monad

import Debug.Trace

-- Definitions

data Camera = Camera ℝ3 ℝ3 ℝ3 ℝ
data Ray    = Ray ℝ3 ℝ3
data Light  = Light ℝ3 ℝ
data Scene  = Scene Obj3 Color [Light] Color

type Color  = PixelRGB8
color r g b = PixelRGB8 r g b
dynamicImage = ImageRGB8

-- Math

d a b = norm (b-a)

instance Multiplicative ℝ Color Color where
	s * (PixelRGB8 a b c) = color (s `mult` a) (s `mult` b) (s `mult` c)
		where 
			bound = max 0 . min 255
			mult a b = fromIntegral . round . bound $ a * (fromIntegral b :: ℝ)

instance Multiplicative Color ℝ Color where
	a * b = b * a

average :: [Color] -> Color
average l = 
	let	
		(rs, gs, bs) = unzip3 $ map 
			(\(PixelRGB8 r g b) -> (fromIntegral r, fromIntegral g, fromIntegral b))
			l :: ([ℝ], [ℝ], [ℝ])
		n = fromIntegral $ length l :: ℝ
		(r, g, b) = (sum rs/n, sum gs/n, sum bs/n)
	in PixelRGB8
		(fromIntegral . round $ r) (fromIntegral . round $ g) (fromIntegral . round $ b)

-- Ray Utilities

cameraRay :: Camera -> ℝ2 -> Ray
cameraRay (Camera p vx vy f) (x,y) =
	let
		v  = vx ⨯ vy
		p' = p + f*v + x*vx + y*vy
		n  = normalized (p' - p)
	in
		Ray p' n

rayFromTo :: ℝ3 -> ℝ3 -> Ray
rayFromTo p1 p2 = Ray p1 (normalized $ p2 - p1)

rayBounds :: Ray -> (ℝ3, ℝ3) -> ℝ2
rayBounds ray box =
	let
		Ray (cPx, cPy, cPz) cameraV@(cVx, cVy, cVz) = ray
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
		step = if aval/(4::ℝ) < res then res/(10 :: ℝ) else res
		a'  = a + step
		a'val = obj (p + a'*v)
	in if a'val < 0
	then 
		let a'' = refine (a,a') (\s -> obj (p+s*v))
		in Just (p + a''*v)
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
refine' 0 (a, b) _ _ = a
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
	in case intersection ray ((a, obj (cameraP + a*cameraV)), b) step obj of
		Just p  -> objColor * (sum $ [0.1] ++ do
			Light lightPos lightIntensity <- lights
			let
				ray'@(Ray _ v) = rayFromTo p lightPos
				v' = normalized v
			guard . not $ intersects ray' ((0, obj p),20) step obj
			let
				pval = obj p
				step = 0.1 :: ℝ
				dirDeriv :: ℝ3 -> ℝ
				dirDeriv v = (obj (p + step*v) - pval)/step
				deriv = (dirDeriv (1,0,0), dirDeriv (0,1,0), dirDeriv (0,0,1))
				normal = normalized $ deriv
				unitV = normalized $ v'
				proj a b = (a⋅b)*b
				dist  = d p lightPos
				illumination = (max 0 (normal ⋅ unitV)) * lightIntensity * ((350 :: ℝ)/dist^2)
				rV = 
					let
						normalComponent = proj v' normal
						parComponent    = v' - normalComponent
					in
						normalComponent - parComponent	
			return $ illumination + (4.5::ℝ)*lightIntensity * ((200 :: ℝ)/dist^2)*(abs $ rV ⋅ cameraV)
			)
		Nothing   -> defaultColor

instance DiscreteAproxable SymbolicObj3 DynamicImage where
	discreteAprox res symbObj = dynamicImage $ generateImage pixelRenderer (round w) (round h)
		where
			(w,h) = (200, 200) :: ℝ2
			obj = getImplicit3 symbObj
			box@((x1,y1,z1), (x2,y2,z2)) = getBox3 symbObj
			av :: ℝ -> ℝ -> ℝ
			av a b = (a+b)/(2::ℝ)
			avY = av y1 y2
			avZ = av z1 z2
			deviation = maximum [abs $ y1 - avY, abs $ y2 - avY, abs $ z1 - avZ, abs $ z2 - avZ]
			camera = Camera (x1-deviation*(2.2::ℝ), avY, avZ) (0, -1, 0) (0,0, -1) 1.0
			lights = [Light (x1-deviation*(1.5::ℝ), y1 - (0.4::ℝ)*(y2-y1), avZ) 2.5]
			scene = Scene obj (PixelRGB8 200 200 210) lights (PixelRGB8 255 255 255	)
			pixelRenderer :: Int -> Int -> Color
			pixelRenderer a b = renderScreen 
				((fromIntegral a :: ℝ)/w - (0.5::ℝ)) ((fromIntegral b :: ℝ)/h - (0.5 ::ℝ))
			renderScreen :: ℝ -> ℝ -> Color
			renderScreen a b =
				let
					ray = cameraRay camera (a,b)
				in 
					average $ [
						traceRay 
							(cameraRay camera ((a,b) + (( 0.25::ℝ)/w, ( 0.25::ℝ)/h)))
							2 box scene{-,
						traceRay 
							(cameraRay camera ((a,b) + ((-0.25::ℝ)/w, ( 0.25::ℝ)/h)))
							0.5 box scene,
						traceRay 
							(cameraRay camera ((a,b) + (( 0.25::ℝ)/w,-( 0.25::ℝ)/h)))
							0.5 box scene,
						traceRay 
							(cameraRay camera ((a,b) + ((-0.25::ℝ)/w,-( 0.25::ℝ)/h)))
							0.5 box scene-}
						]

