{-# LANGUAGE RankNTypes #-}
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use the tearser parallel list comprehension syntax, to avoid having to call zip in the complicated comprehensions below.
{-# LANGUAGE ParallelListComp #-}

-- export getContour and getMesh, which returns the edge of a 2D object, or the surface of a 3D object, respectively.
module Graphics.Implicit.Export.Render (getMesh, getContour) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Prelude((<*>), Monoid, pure, mconcat, (-), ceiling, ($), (+), (*), max, div, tail, fmap, reverse, (.), foldMap, min, Int, (<$>))

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, Obj3, Polyline(..), (⋯/), fromℕtoℝ, fromℕ)

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

-- Set the default types for the numbers in this file.
default (ℕ, Fastℕ, ℝ)




getMesh :: ℝ3 -> ℝ3 -> ℝ3 -> Obj3 -> TriangleMesh
getMesh p1@(V3 x1 y1 z1) p2 res@(V3 xres yres zres) obj =
    let
        -- How much space are we rendering?
        d = p2 - p1

        -- How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        nz :: ℕ
        (V3 nx ny nz) = ceiling `fmap` ( d ⋯/ res)

        -- How big are the steps?
        (V3 rx ry rz) = d ⋯/ (fromℕtoℝ `fmap` V3 nx ny nz)


        stepwise :: ℝ -> ℝ -> ℕ -> ℝ
        stepwise x0 dx n = x0 + dx * fromℕtoℝ n


        -- | performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps=32

        sampled :: Map (ℕ, ℕ, ℕ) ℝ
        sampled = forXYZM nx ny nz $ \xm ym zm ->
          M.singleton (xm, ym, zm) $ obj $
            V3
              (stepwise x1 rx xm)
              (stepwise y1 ry ym)
              (stepwise z1 rz zm)

        sample :: ℕ -> ℕ -> ℕ -> ℝ
        sample mx my mz = sampled M.! (mx, my, mz)

        sampleV3 :: V3 ℕ -> ℝ
        sampleV3 (V3 mx my mz) = sample mx my mz

        bleck :: NFData a => ℕ -> [a] -> [a]
        bleck n x = x `using` parBuffer (max 1 $ div (fromℕ n) forcesteps) rdeepseq

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        mkMidsMap :: (forall a. Lens' (V3 a) a) -> Map (ℕ, ℕ, ℕ) ℝ
        mkMidsMap l =
          forXYZMV3 (V3 nx ny nz & l -~ 1) $ \xm ym zm -> do
            let v = V3 xm ym zm
                stepping   = V3 (stepwise x1 rx) (stepwise y1 ry) (stepwise z1 rz)
                stepped    = stepping <*> v
                stepped_up = stepping <*> fmap (+1) v
            M.singleton (xm, ym, zm) $
              interpolate
                (V2 (view l stepped) $ sampleV3 v)
                (V2 (view l stepped_up) $ sampleV3 $ v & l +~ 1)
                (\x -> obj $ stepped & l .~ x)
                (view l res)

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        midsXMap :: Map (ℕ, ℕ, ℕ) ℝ
        midsXMap = mkMidsMap _x


        midsYMap :: Map (ℕ, ℕ, ℕ) ℝ
        midsYMap = mkMidsMap _y


        midsZMap :: Map (ℕ, ℕ, ℕ) ℝ
        midsZMap = mkMidsMap _z


        forXYZ :: ℕ -> ℕ -> ℕ -> (ℕ -> ℕ -> ℕ -> r) -> [[[r]]]
        forXYZ x y z f = do
          zm <- [0 .. z]
          pure $ do
            ym <- [0 .. y]
            pure $ do
              xm <- [0 .. x]
              pure $ f xm ym zm

        forXYZM :: Monoid m => ℕ -> ℕ -> ℕ -> (ℕ -> ℕ -> ℕ -> m) -> m
        forXYZM x y z f = fold $ do
          zm <- [0 .. z]
          ym <- [0 .. y]
          xm <- [0 .. x]
          pure $ f xm ym zm

        forXYZMV3 :: Monoid m => V3 ℕ -> (ℕ -> ℕ -> ℕ -> m) -> m
        forXYZMV3 (V3 x y z) f = forXYZM x y z f

        unpackV3 :: V3 a -> (a, a, a)
        unpackV3 (V3 a b c) = (a, b, c)


        -- (2) Calculate segments for each side
        mkSegsMap :: (forall a. Lens' (V3 a) a) -> (forall a. Lens' (V3 a) (V2 a)) -> Map (ℕ, ℕ, ℕ) [[ℝ3]]
        mkSegsMap l l' =
          forXYZMV3 (V3 nx ny nz & l' -~ 1) $ \xm ym zm ->
            let stepping = V3 (stepwise x1 rx) (stepwise y1 ry) (stepwise z1 rz)
                v = V3 xm ym zm
                stepped = stepping <*> v
                stepped_up =  stepping <*> fmap (+1) v
                mids = V3 midsXMap midsYMap midsZMap
                midA = view (l' . {- DO NOT SWAP -} _y) mids
                midB = view (l' . {- DO NOT SWAP -} _x) mids
                x0 = view l stepped
             in M.singleton (xm, ym, zm) $
                expandPolyline (\yz -> pure 0 & l .~ x0 & l' .~ yz) {-(injX) (view l stepped)-} <$>
                    getSegs
                      (view l' stepped)
                      (view l' stepped_up)
                      (\yz -> obj $ pure 0 & l .~ x0 & l' .~ yz)
                      ( sampleV3 v
                      , sampleV3 $ v & l' . {- DO NOT SWAP -} _x +~ 1
                      , sampleV3 $ v & l' . {- DO NOT SWAP -} _y +~ 1
                      , sampleV3 $ v & l' +~ 1
                      )
                      ( midA M.! (unpackV3 v)
                      , midA M.! (unpackV3 $ v & l' . {- DO NOT SWAP -} _x +~ 1)
                      , midB M.! (unpackV3 v)
                      , midB M.! (unpackV3 $ v & l' . {- DO NOT SWAP -} _y +~ 1)
                      )

        -- (2) Calculate segments for each side
        segsXMap :: Map (ℕ, ℕ, ℕ) [[ℝ3]]
        segsXMap = mkSegsMap _x _yz

        segsYMap :: Map (ℕ, ℕ, ℕ) [[ℝ3]]
        segsYMap = mkSegsMap _y _xz

        segsZMap :: Map (ℕ, ℕ, ℕ) [[ℝ3]]
        segsZMap = mkSegsMap _z _xy



        -- (3) & (4) : get and tesselate loops
        -- FIXME: hack.
        minres = xres `min` yres `min` zres
        sqTris = bleck (nx + ny + nz) $ do
          forXYZ (nx - 1) (ny - 1) (nz - 1) $ \xm ym zm -> do
            foldMap (tesselateLoop minres obj) $ getLoops $
              mconcat
                [        segsXMap M.! (xm,     ym,     zm)
                , mapR $ segsXMap M.! (xm + 1, ym,     zm)
                , mapR $ segsYMap M.! (xm,     ym,     zm)
                ,        segsYMap M.! (xm,     ym + 1, zm)
                ,        segsZMap M.! (xm,     ym,     zm)
                , mapR $ segsZMap M.! (xm,     ym,     zm + 1)
                ]

    in
      -- (5) merge squares, etc
      mergedSquareTris . fold . fold $ fold sqTris

-- | getContour gets a polyline describing the edge of a 2D object.
getContour :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
getContour p1@(V2 x1 y1) p2 res@(V2 xres yres) obj =
    let
        -- | The size of the region we're being asked to search.
        d = p2 - p1

        -- | How many steps will we take on each axis?
        nx :: ℕ
        ny :: ℕ
        (V2 nx ny) = ceiling `fmap` (d ⋯/ res)

        -- | How big are the steps?
        (V2 rx ry) = d ⋯/ (fromℕtoℝ `fmap` (V2 nx ny))

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
f $* a = \b -> f (V2 a b)
infixr 0 $*

(*$) :: Obj2 -> ℝ -> ℝ -> ℝ
f *$ b = \a -> f (V2 a b)
infixr 0 *$

mapR :: [[ℝ3]] -> [[ℝ3]]
mapR = fmap reverse

