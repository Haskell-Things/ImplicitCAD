-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE ParallelListComp #-}

module Graphics.Implicit.Export.Render where

import Graphics.Implicit.Definitions
import Data.VectorSpace

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

-- (4) We tesselate the loops, using a mixture of triangles and squares

import Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop)

-- (5) We try to merge squares, then turn everything into triangles.

import Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris)

-- Success: This is our mesh.

-- Each step is done in parallel using Control.Parallel.Strategies

import Control.Parallel.Strategies (using, rdeepseq, parListChunk)

-- The actual code is just a bunch of ugly argument passing.
-- Utility functions can be found at the end.

-- For efficiency, we need to avoid looking things up in other lists
-- (since they're 3D, it's an O(n³) operation...). So we need to make
-- our algorithms "flow" along the data structure instead of accessing
-- within it. To do this we use the ParallelListComp GHC extention.

-- We also compute lots of things in advance and pass them in as arguments,
-- to reduce redundant computations.

-- All in all, this is kind of ugly. But it is necessary.

-- Note: As far as the actual results of the rendering algorithm, nothing in
--       this file really matters. All the actual decisions about how to build
--       the mesh are abstracted into the imported files. They are likely what
--       you are interested in.

-- For the 2D case, we need one last thing, cleanLoopsFromSegs:

import Graphics.Implicit.Export.Render.HandlePolylines ( cleanLoopsFromSegs )

getMesh :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh p1@(x1,y1,z1) p2@(_,_,_) res obj =
    let
        (dx,dy,dz) = p2 ^-^ p1

        -- How many steps will we take on each axis?
        nx = ceiling $ dx / res
        ny = ceiling $ dy / res
        nz = ceiling $ dz / res

        rx = dx/fromIntegral nx
        ry = dy/fromIntegral ny
        rz = dz/fromIntegral nz

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

        objV = par3DList (nx+2) (ny+2) (nz+2) $ \x _ y _ z _ -> obj (x 0, y 0, z 0)

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.

        midsZ = [[[
                 interpolate (z0, objX0Y0Z0) (z1, objX0Y0Z1) (appAB obj x0 y0) res
                 | x0 <- pXs |                  objX0Y0Z0 <- objY0Z0 | objX0Y0Z1 <- objY0Z1
                ]| y0 <- pYs |                  objY0Z0 <- objZ0 | objY0Z1 <- objZ1
                ]| z0 <- pZs | z1 <- tail pZs | objZ0   <- objV  | objZ1   <- tail objV
                ] `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

        midsY = [[[
                 interpolate (y0, objX0Y0Z0) (y1, objX0Y1Z0) (appAC obj x0 z0) res
                 | x0 <- pXs |                  objX0Y0Z0 <- objY0Z0 | objX0Y1Z0 <- objY1Z0
                ]| y0 <- pYs | y1 <- tail pYs | objY0Z0 <- objZ0 | objY1Z0 <- tail objZ0
                ]| z0 <- pZs |                  objZ0   <- objV 
                ] `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

        midsX = [[[
                 interpolate (x0, objX0Y0Z0) (x1, objX1Y0Z0) (appBC obj y0 z0) res
                 | x0 <- pXs | x1 <- tail pXs | objX0Y0Z0 <- objY0Z0 | objX1Y0Z0 <- tail objY0Z0
                ]| y0 <- pYs |                  objY0Z0 <- objZ0 
                ]| z0 <- pZs |                  objZ0   <- objV 
                ] `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

        -- Calculate segments for each side

        segsZ = [[[ 
            map2  (inj3 z0) $ getSegs (x0,y0) (x1,y1) (obj **$ z0)
                (objX0Y0Z0, objX1Y0Z0, objX0Y1Z0, objX1Y1Z0)
                (midA0, midA1, midB0, midB1)
             |x0<-pXs|x1<-tail pXs|midB0<-mX'' |midB1<-mX'T    |midA0<-mY'' |midA1<-tail mY''
             |objX0Y0Z0<-objY0Z0|objX1Y0Z0<-tail objY0Z0|objX0Y1Z0<-objY1Z0|objX1Y1Z0<-tail objY1Z0
            ]|y0<-pYs|y1<-tail pYs|mX'' <-mX'  |mX'T <-tail mX'|mY'' <-mY'
             |objY0Z0 <- objZ0 | objY1Z0 <- tail objZ0
            ]|z0<-pZs             |mX'  <-midsX|                mY'  <-midsY
             |objZ0 <- objV
            ] `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

        segsY = [[[ 
            map2  (inj2 y0) $ getSegs (x0,z0) (x1,z1) (obj *$* y0) 
                 (objX0Y0Z0,objX1Y0Z0,objX0Y0Z1,objX1Y0Z1)
                 (midA0, midA1, midB0, midB1)
             |x0<-pXs|x1<-tail pXs|midB0<-mB'' |midB1<-mBT'      |midA0<-mA'' |midA1<-tail mA''
             |objX0Y0Z0<-objY0Z0|objX1Y0Z0<-tail objY0Z0|objX0Y0Z1<-objY0Z1|objX1Y0Z1<-tail objY0Z1
            ]|y0<-pYs|             mB'' <-mB'  |mBT' <-mBT       |mA'' <-mA'
             |objY0Z0 <- objZ0 | objY0Z1 <- objZ1
            ]|z0<-pZs|z1<-tail pZs|mB'  <-midsX|mBT  <-tail midsX|mA'  <-midsZ 
             |objZ0 <- objV | objZ1 <- tail objV
            ] `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

        segsX = 
            [[[ 
            map2  (inj1 x0) $ getSegs (y0,z0) (y1,z1) (obj $** x0) 
                 (objX0Y0Z0,objX0Y1Z0,objX0Y0Z1,objX0Y1Z1)
                 (midA0, midA1, midB0, midB1)
             |x0<-pXs|             midB0<-mB'' |midB1<-mBT'      |midA0<-mA'' |midA1<-mA'T
             |objX0Y0Z0<-objY0Z0|objX0Y1Z0<-    objY1Z0|objX0Y0Z1<-objY0Z1|objX0Y1Z1<-     objY1Z1
            ]|y0<-pYs|y1<-tail pYs|mB'' <-mB'  |mBT' <-mBT       |mA'' <-mA'  |mA'T <-tail mA'
             |objY0Z0  <-objZ0  |objY1Z0  <-tail objZ0  |objY0Z1  <-objZ1  |objY1Z1  <-tail objZ1  
            ]|z0<-pZs|z1<-tail pZs|mB'  <-midsY|mBT  <-tail midsY|mA'  <-midsZ 
             |objZ0 <- objV | objZ1 <- tail objV
            ]  `using` (parListChunk (max 1 $ div nz 32) rdeepseq)

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
            ]
    
    in mergedSquareTris $ concat $ concat $ concat sqTris -- (5) merge squares, etc




getContour :: ℝ2 -> ℝ2 -> ℝ -> Obj2 -> [Polyline]
getContour p1@(x1, y1) p2@(_, _) res obj =
    let
        (dx,dy) = p2 ^-^ p1

        -- How many steps will we take on each axis?
        nx = ceiling $ dx / res
        ny = ceiling $ dy / res

        rx = dx/fromIntegral nx
        ry = dy/fromIntegral ny

        pYs = [ y1 + ry*n | n <- [0.. fromIntegral ny] ]
        pXs = [ x1 + rx*n | n <- [0.. fromIntegral nx] ]


        {-# INLINE par2DList #-}
        par2DList lenx leny f = 
            [[ f
                (\n -> x1 + rx*fromIntegral (mx+n)) mx 
                (\n -> y1 + ry*fromIntegral (my+n)) my
            | mx <- [0..lenx] ] | my <- [0..leny] ]
                `using` (parListChunk (max 1 $ div leny 32) rdeepseq)


        -- Evaluate obj to avoid waste in mids, segs, later.

        objV = par2DList (nx+2) (ny+2) $ \x _ y _ -> obj (x 0, y 0)

        -- (1) Calculate mid poinsts on X, Y, and Z axis in 3D space.

        midsY = [[
                 interpolate (y0, objX0Y0) (y1, objX0Y1) (obj $* x0) res
                 | x0 <- pXs |                  objX0Y0 <- objY0   | objX0Y1 <- objY1
                ]| y0 <- pYs | y1 <- tail pYs | objY0   <- objV    | objY1   <- tail objV
                ] `using` (parListChunk (max 1 $ div ny 32) rdeepseq)

        midsX = [[
                 interpolate (x0, objX0Y0) (x1, objX1Y0) (obj *$ y0) res
                 | x0 <- pXs | x1 <- tail pXs | objX0Y0 <- objY0 | objX1Y0 <- tail objY0
                ]| y0 <- pYs |                  objY0   <- objV 
                ] `using` (parListChunk (max 1 $ div ny 32) rdeepseq)

        -- Calculate segments for each side

        segs = [[ 
            getSegs (x0,y0) (x1,y1) obj
                (objX0Y0, objX1Y0, objX0Y1, objX1Y1)
                (midA0, midA1, midB0, midB1)
             |x0<-pXs|x1<-tail pXs|midB0<-mX'' |midB1<-mX'T    |midA0<-mY'' |midA1<-tail mY''
             |objX0Y0<-objY0|objX1Y0<-tail objY0|objX0Y1<-objY1|objX1Y1<-tail objY1
            ]|y0<-pYs|y1<-tail pYs|mX'' <-midsX|mX'T <-tail midsX|mY'' <-midsY
             |objY0 <- objV  | objY1 <- tail objV
            ] `using` (parListChunk (max 1 $ div ny 32) rdeepseq)

    in cleanLoopsFromSegs $ concat $ concat $ segs -- (5) merge squares, etc




-- silly utility functions

inj1 a (b,c) = (a,b,c)
inj2 b (a,c) = (a,b,c)
inj3 c (a,b) = (a,b,c)

infixr 0 $**
infixr 0 *$*
infixr 0 **$
infixr 0 $*
infixr 0 *$
f $* a = \b -> f (a,b)
f *$ b = \a -> f (a,b)
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
