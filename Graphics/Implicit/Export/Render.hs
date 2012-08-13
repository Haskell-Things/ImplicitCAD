-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE ParallelListComp #-}

module Graphics.Implicit.Export.Render where

import Debug.Trace

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.Definitions

-- Here's the plan for rendering a cube (the 2D case is trivial):

-- (1) We calculate midpoints using interpolate.
--     This guarentees that our mesh will line up everywhere.
--     (Contrast with calculating them in getSegs)

import Graphics.Implicit.Export.Render.Interpolate (interpolate)

-- (2) We calculate the segments separating the inside and outside of our
--     object on the sides of the cube.
--     getSegs internally uses refine from RefineSegs to subdivide the segs
--     to better match the boundary.

import Graphics.Implicit.Export.Render.GetSegs (getSegs)
-- import Graphics.Implicit.Export.Render.RefineSegs (refine)

-- (3) We put the segments from all sides of the cube together
--     and extract closed loops.

import Graphics.Implicit.Export.Render.GetLoops (getLoops)

-- (4) We tesselate the loops, using a mixtur of triangles and squares

import Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop)

-- (5) We try to merge squares, then turn everything into triangles.

import Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris)

-- Success: This is our mesh.

-- Each step is done in parallel using Control.Parallel.Strategies

import Control.Parallel.Strategies (using, rdeepseq, parListChunk)

-- The actual code is just a bunch of ugly argument passing.
-- Utility functions can be found at the end.

getMesh :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh (x1, y1, z1) (x2, y2, z2) res obj = 
	let
		dx = x2-x1
		dy = y2-y1
		dz = z2-z1

		-- How many steps will we take on each axis?
		nx = ceiling $ dx / res
		ny = ceiling $ dy / res
		nz = ceiling $ dz / res

		rx = dx/fromIntegral nx
		ry = dy/fromIntegral ny
		rz = dz/fromIntegral nz

		l ! (a,b,c) = l !! c !! b !! a

		pZs = [ z1 + rz*n | n <- [0.. fromIntegral nz] ]
		pYs = [ y1 + ry*n | n <- [0.. fromIntegral ny] ]
		pXs = [ x1 + rx*n | n <- [0.. fromIntegral nx] ]


		{-# INLINE par3DList #-}
		par3DList lenx leny lenz f = 
			[[[f 
				(\n -> x1 + rx*fromIntegral (mx+n)) mx 
				(\n -> y1 + ry*fromIntegral (my+n)) my 
				(\n -> z1 + rz*fromIntegral (mz+n)) mz
			| mx <- [0..lenx] ] | my <- [0..leny] ] | mz <- [0..lenz] ] 
				`using` (parListChunk (max 1 $ div lenz 32) rdeepseq)


		-- Evaluate obj to avoid waste in mids, segs, later.

		-- objV = par3DList (nx+2) (ny+2) (nz+2) $ \x _ y _ z _ -> obj (x 0, y 0, z 0)

		-- (1) Calculate mid poinsts on X, Y, and Z axis in 3D space.

		midsZ = par3DList (nx+1) (ny+1) (nz+1) $ \x mx y my z mz ->
	          interpolate 
	              (z 0, obj (x 0, y 0, z 0) ) 
	              (z 1, obj (x 0, y 0, z 1) )
	              (appAB obj (x 0) (y 0)) res

		midsY = par3DList (nx+1) (ny+1) (nz+1) $ \x mx y my z mz ->
		      interpolate 
		          (y 0, obj (x 0, y 0, z 0) ) 
		          (y 1, obj (x 0, y 1, z 0) )
		          (appAC obj (x 0) (z 0)) res

		midsX = par3DList (nx+1) (ny+1) (nz+1) $ \x mx y my z mz ->
		      interpolate 
		          (x 0, obj (x 0, y 0, z 0) ) 
		          (x 1, obj (x 1, y 0, z 0) )
		          (appBC obj (y 0) (z 0)) res

		-- Calculate segments for each side

		segsZ = {- [[[ 
			map2  (inj3 z0) $ getSegs (x0,y0) (x1,y1) (obj **$ z0) (midA0, midA1, midB0, midB1)
			 |x0<-pXs|x1<-tail pXs|midB0<-mX'' |midB1<-mX'T    |midA0<-mY'' |midA1<-tail mY''
			]|y0<-pYs|y1<-tail pYs|mX'' <-mX'  |mX'T <-tail mX'|mY'' <-mY'
			]|z0<-pZs             |mX'  <-midsX|                mY'  <-midsY 
			] -}
			{-let
				iteree = zip3D3 (lag3 points) (lag3s22 midsY) (lag3s12 midsX)
				transform (((x0,y0,z0),(x1,y1,z1)), (midA0,midA1), (midB0,midB1)) =
					map2  (inj3 z0) $ 
						getSegs (x0,y0) (x1,y1) (obj **$ z0) (midA0, midA1, midB0, midB1)
				result = for3 iteree transform
			in
				result --`using` (parListChunk (max 1 $ div lenz 32) rdeepseq)-}
		  par3DList (nx-1) (ny-1) (nz) $ \x mx y my z mz ->
		       map2  (inj3 (z 0)) $ getSegs 
		           (x 0, y 0)
                   (x 1, y 1)
                   (obj **$ z 0)
		           (midsY ! (mx, my, mz), midsY ! (mx+1, my, mz),
		            midsX ! (mx, my, mz), midsX ! (mx, my+1, mz)) -- -}
		segsY = {-[[[ 
			map2  (inj2 y0) $ getSegs (x0,z0) (x1,z1) (obj *$* y0) (midA0, midA1, midB0, midB1)
			 |x0<-pXs|x1<-tail pXs|midB0<-mB'' |midB1<-mBT'      |midA0<-mA'' |midA1<-tail mA''
			]|y0<-pYs|             mB'' <-mB'  |mBT' <-mBT       |mA'' <-mA'
			]|z0<-pZs|z1<-tail pZs|mB'  <-midsX|mBT  <-tail midsX|mA'  <-midsZ 
			]-}
			{-let
				iteree = zip3D3 (lag3 points) (lag3s22 midsZ) (lag3s02 midsX)
				transform (((x0,y0,z0),(x1,y1,z1)), (midA0,midA1), (midB0,midB1)) =
					map2  (inj2 y0) $ 
						getSegs (x0,z0) (x1,z1) (obj *$* y0) (midA0, midA1, midB0, midB1)
				result = for3 iteree transform
			in
				result -}
			par3DList (nx-1) (ny) (nz-1) $ \x mx y my z mz ->
		       map2  (inj2 (y 0)) $ getSegs 
		           (x 0, z 0)
                   (x 1, z 1)
                   (obj *$* y 0)
		           (midsZ ! (mx, my, mz), midsZ ! (mx+1, my, mz),
		            midsX ! (mx, my, mz), midsX ! (mx, my, mz+1))
		-- -}

		segsX = 
			{-[[[ 
			map2  (inj1 x0) $ getSegs (y0,z0) (y1,z1) (obj $** x0) (midA0, midA1, midB0, midB1)
			 |x0<-pXs|             midB0<-mB'' |midB1<-mBT'      |midA0<-mA'' |midA1<-mA'T
			]|y0<-pYs|y1<-tail pYs|mB'' <-mB'  |mBT' <-mBT       |mA'' <-mA'  |mA'T <-tail mA'
			]|z0<-pZs|z1<-tail pZs|mB'  <-midsY|mBT  <-tail midsY|mA'  <-midsZ 
			]-}
			{-let
				iteree = zip3D3 (lag3 points) (lag3s12 midsZ) (lag3s02 midsY)
				transform (((x0,y0,z0),(x1,y1,z1)), (midA0,midA1), (midB0,midB1)) =
					map2  (inj1 x0) $ 
						getSegs (y0,z0) (y1,z1) (obj $** x0) (midA0, midA1, midB0, midB1)
				result = for3 iteree transform
			in
				result -}

			par3DList (nx) (ny-1) (nz-1) $ \x mx y my z mz ->
		       map2  (inj1 (x 0)) $ getSegs 
		           (y 0, z 0)
                   (y 1, z 1)
                   (obj $** x 0)
		           (midsZ ! (mx, my, mz), midsZ ! (mx, my+1, mz),
		            midsY ! (mx, my, mz), midsY ! (mx, my, mz+1) ) -- -}

		-- (3) & (4) : get and tesselate loops
 
		sqTris = [[[
		    concat $ map (tesselateLoop res obj) $ getLoops $ concat [
		                segX''',
		           mapR segX''T,
		           mapR segY''',
		                segY'T',
		                segZ''',
		           mapR segZT''
		        ]

			 | segZ'''<- segZ''| segZT''<- segZT'
			 | segY'''<- segY''| segY'T'<- segY'T
			 | segX'''<- segX''| segX''T<- tail segX''

			]| segZ'' <- segZ' | segZT' <- segZT
			 | segY'' <- segY' | segY'T <- tail segY'
			 | segX'' <- segX'

			]| segZ'  <- segsZ | segZT  <- tail segsZ
			 | segY' <- segsY
			 | segX' <- segsX
		       ] `using` (parListChunk (nx*ny*(max 1 $ div nz 32)) rdeepseq)
	
	in mergedSquareTris $ concat $ concat $ concat sqTris -- (5) merge squares, etc



-- silly utility functions

inj1 a (b,c) = (a,b,c)
inj2 b (a,c) = (a,b,c)
inj3 c (a,b) = (a,b,c)

infixr 0 $**
infixr 0 *$*
infixr 0 **$
f $** a = \(b,c) -> f (a,b,c)
f *$* b = \(a,c) -> f (a,b,c)
f **$ c = \(a,b) -> f (a,b,c)

appAB f a b = \c -> f (a,b,c)
appBC f b c = \a -> f (a,b,c)
appAC f a c = \b -> f (a,b,c)

map2 f = map (map f)
map2R f = map (reverse . map f)
mapR = map reverse

{-
lagzip a = zip a (tail a)
tupzip (a,b) = zip a b
tupzip3 (a,b,c) = zip3 a b c

zipD2 a b = map tupzip $ zip a b
zipD3 a b = map (map tupzip) . map tupzip $ zip a b

zip3D3 a b c = map (map tupzip3) . map tupzip3 $ zip3 a b c

lag3s02 = map (map tupzip) . map tupzip . lagzip
lag3s12 = map (map tupzip) . map lagzip
lag3s22 = map (map lagzip)

lag3 :: [[[a]]] -> [[[(a,a)]]]
lag3 a = zipD3 a $ map (map tail) $ map tail $ tail a

for3 = flip (map . map . map)
-}
