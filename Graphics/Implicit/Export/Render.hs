-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use the tearser parallel list comprehension syntax, to avoid having to call zip in the complicated comprehensions below.
{-# LANGUAGE ParallelListComp #-}

-- export getContour and getMesh, which returns the edge of a 2D object, or the surface of a 3D object, respectively.
module Graphics.Implicit.Export.Render (getMesh, getContour) where

import Prelude(pure, mconcat, (-), ceiling, ($), (+), (*), max, div, tail, fmap, reverse, (.), foldMap, min, Int, (<>), (<$>))

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, Obj3, Polyline(Polyline), (⋯/), fromℕtoℝ, fromℕ)

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
import Data.List (zip6, zip7, zip4)
import Data.List (zip3)
import Data.List (zip)

-- Set the default types for the numbers in this file.
default (ℕ, Fastℕ, ℝ)


zip9 :: [a1] -> [a2] -> [a3] -> [a4] -> [a5] -> [a6] -> [a7] -> [a8] -> [a9] ->  [(a1, a2, a3, a4, a5, a6, a7, a8, a9)]
zip9
  (a1 : a1s)
  (a2 : a2s)
  (a3 : a3s)
  (a4 : a4s)
  (a5 : a5s)
  (a6 : a6s)
  (a7 : a7s)
  (a8 : a8s)
  (a9 : a9s)
  = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  : zip9 a1s a2s a3s a4s a5s a6s a7s a8s a9s
zip9 _ _ _ _ _ _ _ _ _ = []

zip10 :: [a1] -> [a2] -> [a3] -> [a4] -> [a5] -> [a6] -> [a7] -> [a8] -> [a9] -> [a10] -> [(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)]
zip10
  (a1 : a1s)
  (a2 : a2s)
  (a3 : a3s)
  (a4 : a4s)
  (a5 : a5s)
  (a6 : a6s)
  (a7 : a7s)
  (a8 : a8s)
  (a9 : a9s)
  (a10 : a10s)
  = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  : zip10 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s
zip10 _ _ _ _ _ _ _ _ _ _ = []




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

        -- The positions we're rendering.
        pXs, pYs, pZs :: [ℝ]
        pXs = fmap (stepwise x1 rx) [0.. nx]
        pYs = fmap (stepwise y1 ry) [0.. ny]
        pZs = fmap (stepwise z1 rz) [0.. nz]


        stepwise :: ℝ -> ℝ -> ℕ -> ℝ
        stepwise x0 dx n = x0 + dx * fromℕtoℝ n


        -- | performance tuning.
        -- FIXME: magic number.
        forcesteps :: Int
        forcesteps=32

        -- | Perform a given function on every point in a 3D grid.
        par3DList :: ℕ -> ℕ -> ℕ -> [[[ℝ]]]
        par3DList lenx leny lenz = bleck (lenx + leny + lenz) $ do
          mz <- [0..lenz]
          pure $ do
            my <- [0..leny]
            pure $ do
              mx <- [0..lenx]
              pure $ sample mx my mz

        sample :: ℕ -> ℕ -> ℕ -> ℝ
        sample mx my mz = obj $
          V3
            (stepwise x1 rx mx)
            (stepwise y1 ry my)
            (stepwise z1 rz mz)

        -- | Evaluate obj to avoid waste in mids, segs, later.
        objV :: [[[ℝ]]]
        objV = par3DList (nx+2) (ny+2) (nz+2)

        bleck :: NFData a => ℕ -> [a] -> [a]
        bleck n x = x `using` parBuffer (max 1 $ div (fromℕ n) forcesteps) rdeepseq

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        midsZ :: [[[ℝ]]]
        midsZ = bleck nz $ do
          zm <- [0 .. nz - 1]
          let z0 = stepwise z1 rz zm
              z1' = stepwise z1 rz (zm + 1)
          pure $ do
            ym <- [0 .. ny]
            let y0 = stepwise y1 ry ym
            pure $ do
              xm <- [0 .. nx]
              let x0 = stepwise x1 rx xm
                  objX0Y0Z0 = sample xm ym zm
                  objX0Y0Z1 = sample xm ym (zm + 1)
              pure $
                interpolate
                  (V2 z0 objX0Y0Z0)
                  (V2 z1' objX0Y0Z1)
                  (appABC obj x0 y0)
                  zres

        midsY :: [[[ℝ]]]
        midsY = bleck ny $ do
          (z0, objZ0) <- zip pZs objV
          pure $ do
            (y0, y1', objY0Z0, objY1Z0) <- zip4 pYs (tail pYs) objZ0 (tail objZ0)
            pure $ do
              (x0, objX0Y0Z0, objX0Y1Z0) <- zip3 pXs objY0Z0 objY1Z0
              pure $
                interpolate
                    (V2 y0 objX0Y0Z0)
                    (V2 y1' objX0Y1Z0)
                    (appACB obj x0 z0)
                    yres


        midsX :: [[[ℝ]]]
        midsX = bleck nx $ do
          (z0, objZ0) <- zip pZs objV
          pure $ do
            (y0,objY0Z0) <- zip pYs objZ0
            pure $ do
              (x0, x1', objX0Y0Z0, objX1Y0Z0) <- zip4 pXs (tail pXs) objY0Z0 (tail objY0Z0)
              pure $
                interpolate
                  (V2 x0 objX0Y0Z0)
                  (V2 x1' objX1Y0Z0)
                  (appBCA obj y0 z0)
                  xres


        -- (2) Calculate segments for each side
        segsZ :: [[[[[ℝ3]]]]]
        segsZ = bleck nz $ do
          (z0, mX', mY', objZ0) <- zip4 pZs midsX midsY objV
          pure $ do
            (y0, y1', mX'', mX'T, mY'', objY0Z0, objY1Z0) <- zip7 pYs (tail pYs) mX' (tail mX') mY' objZ0 (tail objZ0)
            pure $ do
              (x0, x1', midB0, midB1, midA0, midA1, objX0Y0Z0, objX1Y0Z0, objX0Y1Z0, objX1Y1Z0) <-
                zip10 pXs (tail pXs) mX'' mX'T mY'' (tail mY'') objY0Z0 (tail objY0Z0) objY1Z0 (tail objY1Z0)
              pure $
                injZ z0 <$>
                    getSegs
                      (V2 x0 y0)
                      (V2 x1' y1')
                      (obj **$ z0)
                      (objX0Y0Z0, objX1Y0Z0, objX0Y1Z0, objX1Y1Z0)
                      (midA0, midA1, midB0, midB1)


        segsY :: [[[[[ℝ3]]]]]
        segsY = bleck ny $ do
          (z0, z1', mB', mBT, mA', objZ0, objZ1) <-
            zip7 pZs (tail pZs) midsX (tail midsX) midsZ objV (tail objV)
          pure $ do
            (y0, mB'', mBT', mA'', objY0Z0, objY0Z1) <- zip6 pYs mB' mBT mA' objZ0 objZ1
            pure $ do
              (x0, x1', midB0, midB1, midA0, midA1, objX0Y0Z0, objX1Y0Z0, objX0Y0Z1, objX1Y0Z1) <-
                zip10 pXs (tail pXs) mB'' mBT' mA'' (tail mA'') objY0Z0 (tail objY0Z0) objY0Z1 (tail objY0Z1)
              pure $
                injY y0 <$>
                  getSegs
                    (V2 x0 z0)
                    (V2 x1' z1')
                    (obj *$* y0)
                    (objX0Y0Z0, objX1Y0Z0, objX0Y0Z1, objX1Y0Z1)
                    (midA0, midA1, midB0, midB1)



        segsX :: [[[[[ℝ3]]]]]
        segsX = bleck nx $ do
          (z0, z1', mB', mBT, mA', objZ0, objZ1) <-
            zip7 pZs (tail pZs) midsY (tail midsY) midsZ objV (tail objV)
          pure $ do
            (y0, y1', mB'', mBT', mA'', mA'T, objY0Z0, objY1Z0, objY0Z1, objY1Z1)
              <- zip10 pYs (tail pYs) mB' mBT mA' (tail mA') objZ0 (tail objZ0) objZ1 (tail objZ1)
            pure $ do
              (x0, midB0, midB1, midA0, midA1, objX0Y0Z0, objX0Y1Z0, objX0Y0Z1, objX0Y1Z1) <-
                zip9 pXs mB'' mBT' mA'' mA'T objY0Z0 objY1Z0 objY0Z1 objY1Z1
              pure $
                injX x0 <$>
                  getSegs
                    (V2 y0 z0)
                    (V2 y1' z1')
                    (obj $** x0)
                    (objX0Y0Z0, objX0Y1Z0, objX0Y0Z1, objX0Y1Z1)
                    (midA0, midA1, midB0, midB1)


        -- (3) & (4) : get and tesselate loops
        -- FIXME: hack.
        minres = xres `min` yres `min` zres
        sqTris = bleck (nx + ny + nz) $
          [ [ [ foldMap (tesselateLoop minres obj) $ getLoops $
                  mconcat
                    [ segX'''
                    , mapR segX''T
                    , mapR segY'''
                    , segY'T'
                    , segZ'''
                    , mapR segZT''
                    ]
                | segZ''' <- segZ''
                | segZT'' <- segZT'
                | segY''' <- segY''
                | segY'T' <- segY'T
                | segX''' <- segX''
                | segX''T <- tail segX''
                ]
              | segZ'' <- segZ'
              | segZT' <- segZT
              | segY'' <- segY'
              | segY'T <- tail segY'
              | segX'' <- segX'
              ]
            | segZ'  <- segsZ
            | segZT  <- tail segsZ
            | segY'  <- segsY
            | segX'  <- segsX
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

injX :: ℝ -> Polyline -> [ℝ3]
injX a (Polyline xs) = fmap (prepend a) xs
prepend :: ℝ -> ℝ2 -> ℝ3
prepend a (V2 b c) = (V3 a b c)
injY :: ℝ -> Polyline -> [ℝ3]
injY a (Polyline xs) = fmap (insert a) xs
insert :: ℝ -> ℝ2 -> ℝ3
insert b (V2 a c) = (V3 a b c)
injZ :: ℝ -> Polyline -> [ℝ3]
injZ a (Polyline xs) = fmap (postfix a) xs
postfix :: ℝ -> ℝ2 -> ℝ3
postfix c (V2 a b) = (V3 a b c)

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
f $* a = \b -> f (V2 a b)
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

