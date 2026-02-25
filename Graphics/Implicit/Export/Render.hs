-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use the tearser parallel list comprehension syntax, to avoid having to call zip in the complicated comprehensions below.
{-# LANGUAGE ParallelListComp #-}

-- export getContour and getMesh, which returns the edge of a 2D object, or the surface of a 3D object, respectively.
module Graphics.Implicit.Export.Render (getMesh, getContour) where

import Prelude(decodeFloat, encodeFloat, error, snd, (-), ceiling, ($), (+), (*), max, div, fmap, reverse, (.), foldMap, min, Int, (<>), (<$>), traverse, drop, Integer, Integral)

import Graphics.Implicit.Definitions (ℝ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, SymbolicObj2, Obj3, SymbolicObj3, Polyline(getSegments), (⋯/), fromFastℕtoℝ, fromFastℕ, ℝ3' (ℝ3'))

import Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2)

import Graphics.Implicit.Export.Symbolic.Rebound3 (rebound3)

import Graphics.Implicit.ObjectUtil (getBox2, getBox3)

import Data.Foldable(fold)
import Linear ( V3(V3), V2(V2) )

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

-- (3) We put the segments from all sides of the cube together
--     and extract closed loops.
import Graphics.Implicit.Export.Render.GetLoops (getLoops)

-- (4) We tesselate the loops, using a mixture of triangles and squares
import Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop)

-- (5) We try to merge squares, then turn everything into triangles.
import Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris)

-- Success: This is our mesh.

-- Each step on the Z axis is done in parallel using Control.Parallel.Strategies
import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

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
--       the mesh are abstracted into the imported files.

-- For the 2D case, we need one last thing, cleanLoopsFromSegs:
import Graphics.Implicit.Export.Render.HandlePolylines (cleanLoopsFromSegs)
import Data.Maybe (fromMaybe)
import Graphics.Implicit.Primitives (getImplicit)
import Control.Lens (_Wrapped, view, over, _Just)

-- Set the default types for the numbers in this file.
default (Fastℕ, ℝ)

tail :: [a] -> [a]
tail = drop 1

getMesh :: ℝ3 -> SymbolicObj3 -> TriangleMesh
getMesh res@(V3 xres yres zres) symObj =
    let
        -- Grow bounds a little to avoid sampling at exact bounds
        (obj, (p1@(V3 x1 y1 z1), p2)) = rebound3 (getImplicit symObj, getBox3 symObj)

        -- How much space are we rendering?
        d = p2 - p1

        -- How many steps will we take on each axis?
        nx, ny, nz :: Fastℕ
        steps@(V3 nx ny nz) = saferCeiling <$> ( d ⋯/ res)

        -- How big are the steps?
        (V3 rx ry rz) = d ⋯/ (fromFastℕtoℝ <$> steps)

        -- The planes we're rendering along.
        iX = [0.. nx]
        iY = [0.. ny]
        iZ = [0.. nz]
        lastX = tail iX
        lastY = tail iY
        lastZ = tail iZ

        -- Rather than creating a list of values, calculate position on the fly.
        xAt,yAt,zAt :: Fastℕ -> ℝ
        xAt i = x1 + rx * fromFastℕtoℝ i
        yAt i = y1 + ry * fromFastℕtoℝ i
        zAt i = z1 + rz * fromFastℕtoℝ i

        -- performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps = 32

        -- Evaluate obj to avoid waste in mids, segs, later.
        objV = par3DList nx ny nz

        -- Sample our object(s) at every point in the 3D space given.
        par3DList :: Fastℕ -> Fastℕ -> Fastℕ -> [[[ℝ]]]
        par3DList lenx leny lenz =
            [[[ sample mx my mz
            | mx <- [0..lenx] ] | my <- [0..leny] ] | mz <- [0..lenz] ]
              `using` parBuffer (max 1 $ div (fromFastℕ (lenx+leny+lenz)) forcesteps) rdeepseq

        -- sample our object(s) at the given point.
        sample :: Fastℕ -> Fastℕ -> Fastℕ -> ℝ
        sample mx my mz = obj $
              V3
                (x1 + rx*fromFastℕtoℝ mx)
                (y1 + ry*fromFastℕtoℝ my)
                (z1 + rz*fromFastℕtoℝ mz)

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        midsZ = [[[
                 interpolate (V2 (zAt k0) objX0Y0Z0) (V2 (zAt k1) objX0Y0Z1) (appABC obj (xAt i0) (yAt j0)) zres
                 | i0 <- iX |               objX0Y0Z0 <- objY0Z0 | objX0Y0Z1 <- objY0Z1
                ]| j0 <- iY |               objY0Z0   <- objZ0   | objY0Z1   <- objZ1
                ]| k0 <- iZ | k1 <- lastZ | objZ0     <- objV    | objZ1     <- tail objV
                ] `using` parBuffer (max 1 $ div (fromFastℕ nz) forcesteps) rdeepseq

        midsY = [[[
                 interpolate (V2 (yAt j0) objX0Y0Z0) (V2 (yAt j1) objX0Y1Z0) (appACB obj (xAt i0) (zAt k0)) yres
                 | i0 <- iX |               objX0Y0Z0 <- objY0Z0 | objX0Y1Z0 <- objY1Z0
                ]| j0 <- iY | j1 <- lastY | objY0Z0   <- objZ0   | objY1Z0   <- tail objZ0
                ]| k0 <- iZ |               objZ0     <- objV
                ] `using` parBuffer (max 1 $ div (fromFastℕ ny) forcesteps) rdeepseq

        midsX = [[[
                 interpolate (V2 (xAt i0) objX0Y0Z0) (V2 (xAt i1) objX1Y0Z0) (appBCA obj (yAt j0) (zAt k0)) xres
                 | i0 <- iX | i1 <- lastX | objX0Y0Z0 <- objY0Z0 | objX1Y0Z0 <- tail objY0Z0
                ]| j0 <- iY |               objY0Z0   <- objZ0
                ]| k0 <- iZ |               objZ0     <- objV
                ] `using` parBuffer (max 1 $ div (fromFastℕ nx) forcesteps) rdeepseq

        -- (2) Calculate segments for each side
        segsZ = [[[
            injZ (zAt k0) <$> getSegs (V2 (xAt i0) (yAt j0)) (V2 (xAt i1) (yAt j1)) (obj **$ zAt k0) (objX0Y0Z0, objX1Y0Z0, objX0Y1Z0, objX1Y1Z0) (midA0, midA1, midB0, midB1)
             | i0 <- iX | i1 <- lastX | midB0<-mX''  | midB1<-mX'T     | midA0<-mY'' | midA1<-tail mY''  | objX0Y0Z0<-objY0Z0 | objX1Y0Z0<- tail objY0Z0 | objX0Y1Z0<-objY1Z0    | objX1Y1Z0<-tail objY1Z0
            ]| j0 <- iY | j1 <- lastY | mX'' <-mX'   | mX'T <-tail mX' | mY'' <-mY'                      | objY0Z0  <-objZ0                              | objY1Z0  <-tail objZ0
            ]| k0 <- iZ               | mX'  <-midsX |                   mY'  <-midsY                    | objZ0    <-objV
            ] `using` parBuffer (max 1 $ div (fromFastℕ nz) forcesteps) rdeepseq

        segsY = [[[
            injY (yAt j0) <$> getSegs (V2 (xAt i0) (zAt k0)) (V2 (xAt i1) (zAt k1)) (obj *$* yAt j0) (objX0Y0Z0, objX1Y0Z0, objX0Y0Z1, objX1Y0Z1) (midA0, midA1, midB0, midB1)
             | i0 <- iX | i1 <- lastX | midB0<-mB''  | midB1<-mBT'       | midA0<-mA''  | midA1<-tail mA'' | objX0Y0Z0<-objY0Z0 | objX1Y0Z0<-tail objY0Z0 | objX0Y0Z1<-objY0Z1   | objX1Y0Z1<-tail objY0Z1
            ]| j0 <- iY |               mB'' <-mB'   | mBT' <-mBT        | mA'' <-mA'                      | objY0Z0  <-objZ0                             | objY0Z1  <-objZ1
            ]| k0 <- iZ | k1 <- lastZ | mB'  <-midsX | mBT  <-tail midsX | mA'  <-midsZ                    | objZ0    <-objV                              | objZ1    <-tail objV
            ] `using` parBuffer (max 1 $ div (fromFastℕ ny) forcesteps) rdeepseq

        segsX = [[[
            injX (xAt i0) <$> getSegs (V2 (yAt j0) (zAt k0)) (V2 (yAt j1) (zAt k1)) (obj $** xAt i0) (objX0Y0Z0, objX0Y1Z0, objX0Y0Z1, objX0Y1Z1) (midA0, midA1, midB0, midB1)
             | i0 <- iX |               midB0<-mB''  | midB1<-mBT'       | midA0<-mA''  | midA1<-mA'T     | objX0Y0Z0<-objY0Z0 | objX0Y1Z0<-objY1Z0    | objX0Y0Z1<-objY0Z1    | objX0Y1Z1<-     objY1Z1
            ]| j0 <- iY | j1 <- lastY | mB'' <-mB'   | mBT' <-mBT        | mA'' <-mA'   | mA'T <-tail mA' | objY0Z0  <-objZ0   | objY1Z0  <-tail objZ0 | objY0Z1  <-objZ1      | objY1Z1  <-tail objZ1
            ]| k0 <- iZ | k1 <- lastZ | mB'  <-midsY | mBT  <-tail midsY | mA'  <-midsZ                   | objZ0    <- objV                           | objZ1    <- tail objV
            ] `using` parBuffer (max 1 $ div (fromFastℕ nx) forcesteps) rdeepseq

        -- (3) & (4) : get and tesselate loops
        -- FIXME: hack.
        minres = xres `min` yres `min` zres
        sqTris = [[[
            foldMap (tesselateLoop minres obj) $
              fromMaybe (error "unclosed loop in paths given") $
              -- Shove the ℝ3s into ℝ3's to get the NaN checks, then
              -- unwrap everything. This should mostly compile away
              -- given that it is lensy and passing a newtype instance
              -- around. `getLoops` is the function actually doing the
              -- work we care about
              over (_Just . traverse . traverse . traverse) (view _Wrapped) . getLoops . over (traverse . traverse) ℝ3' $
                        segX''' <>
                   mapR segX''T <>
                   mapR segY''' <>
                        segY'T' <>
                        segZ''' <>
                   mapR segZT''
             | segZ'''<- segZ''| segZT''<- segZT'
             | segY'''<- segY''| segY'T'<- segY'T
             | segX'''<- segX''| segX''T<- tail segX''

            ]| segZ'' <- segZ' | segZT' <- segZT
             | segY'' <- segY' | segY'T <- tail segY'
             | segX'' <- segX'

            ]| segZ'  <- segsZ | segZT  <- tail segsZ
             | segY'  <- segsY
             | segX'  <- segsX
            ] `using` parBuffer (max 1 $ div (fromFastℕ $ nx+ny+nz) forcesteps) rdeepseq

    in
      -- (5) merge squares, etc
      mergedSquareTris . fold . fold $ fold sqTris

-- | getContour gets a polyline describing the edge of a 2D object.
getContour :: ℝ2 -> SymbolicObj2 -> [Polyline]
getContour res@(V2 xres yres) symObj =
    let
        -- Grow bounds a little to avoid sampling at exact bounds
        (obj, (p1@(V2 x1 y1), p2)) = rebound2 (getImplicit symObj, getBox2 symObj)

        -- The size of the region we're being asked to search.
        d = p2 - p1

        -- How many steps will we take on each axis?
        nx, ny :: Fastℕ
        steps@(V2 nx ny) = saferCeiling <$> (d ⋯/ res)

        -- How big are the steps?
        (V2 rx ry) = d ⋯/ (fromFastℕtoℝ <$> steps)

        -- The lines we are rendering along.
        iX = [0.. nx]
        iY = [0.. ny]
        lastX = tail iX
        lastY = tail iY

        -- Rather than creating a list of values, calculate position on the fly.
        xAt,yAt :: Fastℕ -> ℝ
        xAt i = x1 + rx * fromFastℕtoℝ i
        yAt i = y1 + ry * fromFastℕtoℝ i

        -- Performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps = 32

        -- Evaluate obj to avoid waste in mids, segs, later.
        objV = par2DList nx ny

        -- Sample our object(s) at every point in the 2D plane given.
        par2DList :: Fastℕ -> Fastℕ -> [[ℝ]]
        par2DList lenx leny =
            [[ sample mx my
                  | mx <- [0..lenx]
                ] | my <- [0..leny]
                ] `using` parBuffer (max 1 $ div (fromFastℕ $ lenx+leny) forcesteps) rdeepseq

        -- sample our object(s) at the given point.
        sample :: Fastℕ -> Fastℕ -> ℝ
        sample mx my = obj $
          V2
                (x1 + rx*fromFastℕtoℝ mx)
                (y1 + ry*fromFastℕtoℝ my)

        -- Calculate mid points on X axis in 2D space.
        midsX = [[
                 interpolate (V2 (xAt i0) objX0Y0) (V2 (xAt i1) objX1Y0) (obj *$ yAt j0) xres
                 | i0 <- iX | i1 <- lastX      | objX0Y0 <- objY0 | objX1Y0 <- tail objY0
                ]| j0 <- iY |                    objY0   <- objV
                ] `using` parBuffer (max 1 $ div (fromFastℕ nx) forcesteps) rdeepseq

        -- Calculate mid points on Y axis in 2D space.
        midsY = [[
                 interpolate (V2 (yAt j0) objX0Y0) (V2 (yAt j1) objX0Y1) (obj $* xAt i0) yres
                 | i0 <- iX |                    objX0Y0 <- objY0 | objX0Y1 <- objY1
                ]| j0 <- iY | j1 <- lastY      | objY0   <- objV  | objY1   <- tail objV
                ] `using` parBuffer (max 1 $ div (fromFastℕ ny) forcesteps) rdeepseq

        -- Calculate segments for each side
        segs = [[
                getSegs (V2 (xAt i0) (yAt j0)) (V2 (xAt i1) (yAt j1)) obj (objX0Y0, objX1Y0, objX0Y1, objX1Y1) (midA0, midA1, midB0, midB1)
                | i0 <- iX | i1 <- lastX | midB0<-mX''  | midB1<-mX'T       | midA0<-mY''  | midA1<-tail mY'' | objX0Y0<-objY0 | objX1Y0<-tail objY0 | objX0Y1<-objY1 | objX1Y1<-tail objY1
               ]| j0 <- iY | j1 <- lastY | mX'' <-midsX | mX'T <-tail midsX | mY'' <-midsY                    | objY0 <- objV                        | objY1 <- tail objV
               ] `using` parBuffer (max 1 $ div (fromFastℕ $ nx+ny) forcesteps) rdeepseq
    in
      -- Merge squares
      cleanLoopsFromSegs . fold $ fold segs

-- utility functions

injX :: ℝ -> Polyline -> [ℝ3]
injX val polyline = prepend val <$> getSegments polyline
  where
    prepend :: ℝ -> ℝ2 -> ℝ3
    prepend a (V2 b c) = V3 a b c

injY :: ℝ -> Polyline -> [ℝ3]
injY val polyline = insert val  <$> getSegments polyline
  where
    insert :: ℝ -> ℝ2 -> ℝ3
    insert b (V2 a c) = V3 a b c

injZ :: ℝ -> Polyline -> [ℝ3]
injZ val polyline = postfix val <$> getSegments polyline
  where
    postfix :: ℝ -> ℝ2 -> ℝ3
    postfix c (V2 a b) = V3 a b c

($**) :: Obj3 -> ℝ -> ℝ2 -> ℝ
f $** a = \(V2 b c) -> f (V3 a b c)
infixr 0 $**

(*$*) :: Obj3 -> ℝ -> ℝ2 -> ℝ
f *$* b = \(V2 a c) -> f (V3 a b c)
infixr 0 *$*

(**$) :: Obj3 -> ℝ -> ℝ2 -> ℝ
f **$ c = \(V2 a b) -> f (V3 a b c)
infixr 0 **$

($*) :: Obj2 -> ℝ -> ℝ -> ℝ
f $* a = f . V2 a
infixr 0 $*

(*$) :: Obj2 -> ℝ -> ℝ -> ℝ
f *$ b = \a -> f (V2 a b)
infixr 0 *$

appABC :: Obj3 -> ℝ -> ℝ -> ℝ -> ℝ
appABC f a b c = f (V3 a b c)
appBCA :: Obj3 -> ℝ -> ℝ -> ℝ -> ℝ
appBCA f b c a = f (V3 a b c)
appACB :: Obj3 -> ℝ -> ℝ -> ℝ -> ℝ
appACB f a c b = f (V3 a b c)

mapR :: [[ℝ3]] -> [[ℝ3]]
mapR = fmap reverse

saferCeiling :: Integral a => ℝ -> a
saferCeiling v = ceiling (v-eps)
  where
   eps :: ℝ
   eps = encodeFloat reps (snd (decodeFloat v) - 52)
   reps :: Integer
   reps = 16

