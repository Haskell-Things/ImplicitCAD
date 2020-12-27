-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}

-- Allow us to use the tearser parallel list comprehension syntax, to avoid having to call zip in the complicated comprehensions below.
{-# LANGUAGE ParallelListComp #-}

-- export getContour and getMesh, which returns the edge of a 2D object, or the surface of a 3D object, respectively.
module Graphics.Implicit.Export.Render (getMesh, getContour) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Prelude

import Control.DeepSeq (force)
import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, Obj3, Polyline(..), (⋯/), fromℕtoℝ, fromℕ)
import Prelude(error, (-), ceiling, ($), (+), (*), max, div, tail, fmap, reverse, (.), foldMap, min, Int, (<>), (<$>))

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, SymbolicObj2, Obj3, SymbolicObj3, Polyline(Polyline), (⋯/), fromℕtoℝ, fromℕ)

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
import Control.Parallel.Strategies (NFData, using, rdeepseq, parBuffer)

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
import Control.Lens (Lens', (-~), view, (.~), (+~), (&))
import Linear (_x, _y, _z, _yz, _xz, _xy)
import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq))
import Data.Maybe (fromMaybe)
import Graphics.Implicit.Primitives (getImplicit)
import qualified Data.Vector.Unboxed as V

-- Set the default types for the numbers in this file.
default (ℕ, Fastℕ, ℝ)

getMesh :: ℝ3 -> SymbolicObj3 -> TriangleMesh
getMesh res@(V3 xres yres zres) symObj =
    let
        -- Grow bounds a little to avoid sampling at exact bounds
        (obj, (p1@(V3 x1 y1 z1), p2)) = rebound3 (getImplicit symObj, getBox3 symObj)

        -- How much space are we rendering?
        d = p2 - p1

        -- How many steps will we take on each axis?
        nx, ny, nz :: Int
        (V3 nx ny nz) = fmap (ceiling) ( d ⋯/ res)

        -- How big are the steps?
        (V3 rx ry rz) = d ⋯/ (fromIntegral `fmap` V3 nx ny nz)


        stepwise :: ℝ -> ℝ -> Int -> ℝ
        stepwise x0 dx n = x0 + dx * fromIntegral n


        -- | performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps=32

        sampled :: V.Vector ℝ
        sampled = mkVector nx ny nz $ \xm ym zm ->
           obj $
              V3
                (stepwise x1 rx xm)
                (stepwise y1 ry ym)
                (stepwise z1 rz zm)

        mkVector :: V.Unbox a => Int -> Int -> Int -> (Int -> Int -> Int -> a) -> V.Vector a
        mkVector x y z f = V.generate ((x + 1) * (y + 1) * (z + 1)) $ \ix ->
          let (xm, ym, zm) = unindex x y z ix
           in f xm ym zm
        {-# INLINE mkVector #-}

        mkVectorV3 :: V.Unbox a => V3 Int -> (V3 Int -> a) -> V.Vector a
        mkVectorV3 (V3 x y z) f = mkVector x y z $ \ix iy iz -> f (V3 ix iy iz)
        {-# INLINE mkVectorV3 #-}

        sample :: Int -> Int -> Int -> ℝ
        sample = lookupIndex nx ny nz sampled

        sampleV3 :: V3 Int -> ℝ
        sampleV3 (V3 mx my mz) = sample mx my mz

        bleck :: NFData a => Int -> [a] -> [a]
        bleck n x = x `using` parBuffer (max 1 $ div n forcesteps) rdeepseq

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        mkMidsMap :: (forall a. Lens' (V3 a) a) -> Map (V3 Int) ℝ
        mkMidsMap l =
          forXYZMV3 (V3 nx ny nz & l -~ 1) $ \xm ym zm -> do
            let v = V3 xm ym zm
                stepping   = V3 (stepwise x1 rx) (stepwise y1 ry) (stepwise z1 rz)
                stepped    = stepping <*> v
                stepped_up = stepping <*> fmap (+1) v
            M.singleton v $
              interpolate
                (V2 (view l stepped) $ sampleV3 v)
                (V2 (view l stepped_up) $ sampleV3 $ v & l +~ 1)
                (\x -> obj $ stepped & l .~ x)
                (view l res)

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        midsXMap :: Map (V3 Int) ℝ
        midsXMap = mkMidsMap _x


        midsYMap :: Map (V3 Int) ℝ
        midsYMap = mkMidsMap _y


        midsZMap :: Map (V3 Int) ℝ
        midsZMap = mkMidsMap _z


        forXYZM :: Monoid m => Int -> Int -> Int -> (Int -> Int -> Int -> m) -> m
        forXYZM x y z f = fold $ do
          zm <- [0 .. z]
          ym <- [0 .. y]
          xm <- [0 .. x]
          pure $ f xm ym zm

        forXYZMV3 :: Monoid m => V3 Int -> (Int -> Int -> Int -> m) -> m
        forXYZMV3 (V3 x y z) f = forXYZM x y z f


        -- (2) Calculate segments for each side
        mkSegsMap
            :: (forall a. Lens' (V3 a) a)
            -> (forall a. Lens' (V3 a) (V2 a))
           -> Int -> Int -> Int
            -> [[ℝ3]]
        mkSegsMap l l' xm ym zm =
            let stepping = V3 (stepwise x1 rx) (stepwise y1 ry) (stepwise z1 rz)
                v = V3 xm ym zm
                stepped = stepping <*> v
                stepped_up =  stepping <*> fmap (+1) v
                mids = V3 midsXMap midsYMap midsZMap
                midA = view (l' . _y) mids
                midB = view (l' . _x) mids
                x0 = view l stepped
             in expandPolyline (\yz -> pure 0 & l .~ x0 & l' .~ yz) <$>
                    getSegs
                      (view l' stepped)
                      (view l' stepped_up)
                      (\yz -> obj $ pure 0 & l .~ x0 & l' .~ yz)
                      ( sampleV3 v
                      , sampleV3 $ v & l' . _x +~ 1
                      , sampleV3 $ v & l' . _y +~ 1
                      , sampleV3 $ v & l' +~ 1
                      )
                      ( midA M.! v
                      , midA M.! (v & l' . _x +~ 1)
                      , midB M.! v
                      , midB M.! (v & l' . _y +~ 1)
                      )



        -- (3) & (4) : get and tesselate loops
        -- FIXME: hack.
        minres = xres `min` yres `min` zres
        sqTris :: [TriSquare]
        sqTris = force $ bleck (nx + ny + nz) $ do
          let segsXMap = mkSegsMap _x _yz
              segsYMap = mkSegsMap _y _xz
              segsZMap = mkSegsMap _z _xy
          forXYZM (nx - 1) (ny - 1) (nz - 1) $ \xm ym zm -> do
            foldMap (tesselateLoop minres obj) $ fromMaybe (error "unclosed loop in paths given") $ getLoops $
              mconcat
                [        segsXMap xm       ym       zm
                , mapR $ segsXMap (xm + 1) ym       zm
                , mapR $ segsYMap xm       ym       zm
                ,        segsYMap xm       (ym + 1) zm
                ,        segsZMap xm       ym       zm
                , mapR $ segsZMap xm       ym       (zm + 1)
                ]

    in
      -- (5) merge squares, etc
      mergedSquareTris sqTris

-- | getContour gets a polyline describing the edge of a 2D object.
getContour :: ℝ2 -> SymbolicObj2 -> [Polyline]
getContour res@(V2 xres yres) symObj =
    let
        -- Grow bounds a little to avoid sampling at exact bounds
        (obj, (p1@(V2 x1 y1), p2)) = rebound2 (getImplicit symObj, getBox2 symObj)

        -- | The size of the region we're being asked to search.
        d = p2 - p1

        -- | How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        (V2 nx ny) = ceiling `fmap` (d ⋯/ res)

        -- | How big are the steps?
        (V2 rx ry) = d ⋯/ (fromℕtoℝ `fmap` V2 nx ny)

        -- The points inside of the region.
        pYs = [ y1 + ry*fromℕtoℝ p | p <- [0.. ny] ]
        pXs = [ x1 + rx*fromℕtoℝ p | p <- [0.. nx] ]

        -- | Performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps=32

        par2DList :: ℕ -> ℕ -> ((ℕ -> ℝ) -> ℕ -> (ℕ -> ℝ) -> ℕ -> ℝ) -> [[ℝ]]
        par2DList lenx leny f =
            [[ f
                (\n -> x1 + rx*fromℕtoℝ (mx+n)) mx
                (\n -> y1 + ry*fromℕtoℝ (my+n)) my
                  | mx <- [0..lenx]
                ] | my <- [0..leny]
                ] `using` parBuffer (max 1 $ div (fromℕ $ lenx+leny) forcesteps) rdeepseq

        -- | Fully evaluate obj to avoid waste in mids, segs, later.
        objV = par2DList (nx+2) (ny+2) $ \x _ y _ -> obj (V2 (x 0) (y 0))

        -- | Calculate mid points on X, and Y axis in 2D space.
        midsY = [[
                 interpolate (V2 y0 objX0Y0) (V2 y1' objX0Y1) (obj $* x0) yres
                 | x0 <- pXs |                   objX0Y0 <- objY0   | objX0Y1 <- objY1
                ]| y0 <- pYs | y1' <- tail pYs | objY0   <- objV    | objY1   <- tail objV
                ] `using` parBuffer (max 1 $ div (fromℕ ny) forcesteps) rdeepseq

        midsX = [[
                 interpolate (V2 x0 objX0Y0) (V2 x1' objX1Y0) (obj *$ y0) xres
                 | x0 <- pXs | x1' <- tail pXs | objX0Y0 <- objY0 | objX1Y0 <- tail objY0
                ]| y0 <- pYs |                   objY0   <- objV
                ] `using` parBuffer (max 1 $ div (fromℕ nx) forcesteps) rdeepseq

        -- | Calculate segments for each side
        segs = [[
            getSegs (V2 x0 y0) (V2 x1' y1') obj (objX0Y0, objX1Y0, objX0Y1, objX1Y1) (midA0, midA1, midB0, midB1)
             | x0<-pXs | x1'<-tail pXs |midB0<-mX''  | midB1<-mX'T       | midA0<-mY''  | midA1<-tail mY'' | objX0Y0<-objY0 | objX1Y0<-tail objY0 | objX0Y1<-objY1 | objX1Y1<-tail objY1
            ]| y0<-pYs | y1'<-tail pYs |mX'' <-midsX | mX'T <-tail midsX | mY'' <-midsY                    | objY0 <- objV                        | objY1 <- tail objV
            ] `using` parBuffer (max 1 $ div (fromℕ $ nx+ny) forcesteps) rdeepseq
    in
      -- Merge squares
      cleanLoopsFromSegs . fold $ fold segs

-- utility functions

expandPolyline :: (ℝ2 -> ℝ3) -> Polyline -> [ℝ3]
expandPolyline f = fmap f . getPolyline

($*) :: Obj2 -> ℝ -> ℝ -> ℝ
f $* a = f . V2 a
infixr 0 $*

(*$) :: Obj2 -> ℝ -> ℝ -> ℝ
f *$ b = \a -> f (V2 a b)
infixr 0 *$

mapR :: [[ℝ3]] -> [[ℝ3]]
mapR = fmap reverse

unindex :: Int -> Int -> Int -> Int -> (Int, Int, Int)
unindex nx ny _nz ix =
  let x = ix `mod` nx
      y = (ix `div` nx) `mod` ny
      z = ix `div` (nx * ny)
   in (x, y, z)
{-# INLINE unindex #-}

bigIndex :: Int -> Int -> Int -> Int -> Int -> Int -> Int
bigIndex nx ny _nz =
  let mz = ny * nx
   in \ix iy iz -> iz * mz + iy * nx + ix
{-# INLINE bigIndex #-}

lookupIndex :: V.Unbox a => Int -> Int -> Int -> V.Vector a -> Int -> Int -> Int -> a
lookupIndex nx ny nz v =
  let ix = bigIndex nx ny nz
   in \x y z -> v V.! ix x y z
{-# INLINE lookupIndex #-}




