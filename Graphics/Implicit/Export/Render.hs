-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render where

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

		segsZ = par3DList (nx-1) (ny-1) (nz) $ \x mx y my z mz ->
		       map2  (inj3 (z 0)) $ getSegs 
		           (x 0, y 0)
                   (x 1, y 1)
                   (obj **$ z 0)
		           (midsY ! (mx, my, mz), midsY ! (mx+1, my, mz),
		            midsX ! (mx, my, mz), midsX ! (mx, my+1, mz))

		segsY = par3DList (nx-1) (ny) (nz-1) $ \x mx y my z mz ->
		       map2  (inj2 (y 0)) $ getSegs 
		           (x 0, z 0)
                   (x 1, z 1)
                   (obj *$* y 0)
		           (midsZ ! (mx, my, mz), midsZ ! (mx+1, my, mz),
		            midsX ! (mx, my, mz), midsX ! (mx, my, mz+1))

		segsX = par3DList (nx) (ny-1) (nz-1) $ \x mx y my z mz ->
		       map2  (inj1 (x 0)) $ getSegs 
		           (y 0, z 0)
                   (y 1, z 1)
                   (obj $** x 0)
		           (midsZ ! (mx, my, mz), midsZ ! (mx, my+1, mz),
		            midsY ! (mx, my, mz), midsY ! (mx, my, mz+1) )

		-- (3) & (4) : get and tesselate loops
 
		sqTris = [
		        concat $ map (tesselateLoop res obj) $ getLoops $ concat [
		                     segsX !! mz     !! my     !! mx,
		              mapR $ segsX !! mz     !! my     !!(mx + 1),
		              mapR $ segsY !! mz     !! my     !! mx,
		                     segsY !! mz     !!(my + 1)!! mx,
		                     segsZ !! mz     !! my     !! mx,
		              mapR $ segsZ !!(mz + 1)!! my     !! mx
		          ]
		       | mx <- [0.. nx-1], my <- [0.. ny-1], mz <- [0.. nz-1]
		       ] `using` (parListChunk (nx*ny*(max 1 $ div nz 32)) rdeepseq)
	
	in mergedSquareTris $ concat sqTris -- (5) merge squares, etc



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


