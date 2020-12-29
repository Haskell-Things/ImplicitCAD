-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE RankNTypes #-}

-- Allow us to use the tearser parallel list comprehension syntax, to avoid having to call zip in the complicated comprehensions below.
{-# LANGUAGE ParallelListComp #-}

-- export getContour and getMesh, which returns the edge of a 2D object, or the surface of a 3D object, respectively.
module Graphics.Implicit.Export.Render (getMesh, getContour) where

import Prelude (Monoid, pure, mconcat, (<*>), fromIntegral, error, (-), ceiling, ($), (+), (*), max, div, tail, fmap, reverse, (.), foldMap, min, Int, (<$>))

import Control.Lens (Lens', (-~), view, (.~), (+~), (&))
import Data.Foldable(fold)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed.V3 as V

import Graphics.Implicit.Primitives (getImplicit)

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, ℝ2, ℝ3, TriangleMesh, Obj2, Polyline(getPolyline), (⋯/), fromℕtoℝ, fromℕ)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)

import Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2)

import Graphics.Implicit.Export.Symbolic.Rebound3 (rebound3)

import Graphics.Implicit.ObjectUtil (getBox2, getBox3)

import Graphics.Implicit.Export.Render.Definitions (TriSquare)
import Linear ( V3(V3), V2(V2), _x, _y, _z, _yz, _xz, _xy)

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
        steps@(V3 nx ny nz) = fmap ceiling ( d ⋯/ res)

        -- How big are the steps?
        (V3 rx ry rz) = d ⋯/ fmap fromIntegral steps

        -- Add @n@ steps of size @dx@ to @x0@
        stepwise :: ℝ -> ℝ -> Int -> ℝ
        stepwise x0 dx n = x0 + dx * fromIntegral n
        {-# INLINE stepwise #-}

        -- Get a point in space by running @stepwise@ in every dimension
        stepping :: V3 (Int -> ℝ)
        stepping = V3 (stepwise x1 rx) (stepwise y1 ry) (stepwise z1 rz)

        -- Memoized access to @obj@
        sample :: V3 Int -> ℝ
        sample = V.memoize steps $ \v -> obj $ stepping <*> v
        {-# NOINLINE sample #-}  -- don't inline it, or we lose our memoization

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        mkMids :: (forall a. Lens' (V3 a) a) -> V.VectorV3 ℝ
        mkMids l =
          V.mkVectorV3 (steps & l -~ 1) $ \v -> do
            let stepped    = stepping <*> v
                stepped_up = stepping <*> (v + 1)
            interpolate
                (V2 (view l stepped) $ sample v)
                (V2 (view l stepped_up) $ sample $ v & l +~ 1)
                (\x -> obj $ stepped & l .~ x)
                (view l res)

        -- (1) Calculate mid points on X, Y, and Z axis in 3D space.
        midsX, midsY, midsZ :: V.VectorV3 ℝ
        midsX = mkMids _x
        midsY = mkMids _y
        midsZ = mkMids _z

        -- (2) Calculate segments for each side
        mkSegs
            :: (forall a. Lens' (V3 a) a)
               -- ^ A lens towards the dimension we're building segments for
            -> (forall a. Lens' (V3 a) (V2 a))
               -- ^ A lens selecting the other two dimensions, in lexographical
               -- order --- that is, use @_xy@ instead of @_yx@.
           -> V3 Int
            -> [[ℝ3]]
        mkSegs l l' =
          let mids = V3 midsX midsY midsZ
              midA = view (l' . _y) mids  -- '_y' here refers to the second
                                          -- component of the 'V2' selected by
                                          -- @l'@ --- which is to say, it
                                          -- selects the Z dimension when
                                          -- @l' = _xz@
              midB = view (l' . _x) mids
           in \v ->
                 let
                     -- the position in space for index @v@
                     stepped    = stepping <*> v
                     -- the next position in space, stepwise
                     stepped_up = stepping <*> (v + 1)
                     x0 = view l stepped

                     -- | Transform a vector in R2 into one in R3 by setting
                     -- its @l@-focused dimension to @x0@.
                     injectDimension :: ℝ2 -> ℝ3
                     injectDimension yz = 0 & l .~ x0 & l' .~ yz
                  in expandPolyline injectDimension <$>
                        getSegs
                          (view l' stepped)
                          (view l' stepped_up)
                          (\yz -> obj $ 0 & l .~ x0 & l' .~ yz)
                          ( sample v
                          , sample $ v & l' . _x +~ 1
                          , sample $ v & l' . _y +~ 1
                          , sample $ v & l' +~ 1
                          )
                          ( midA V.! v
                          , midA V.! (v & l' . _x +~ 1)
                          , midB V.! v
                          , midB V.! (v & l' . _y +~ 1)
                          )
        {-# INLINE mkSegs #-}

        -- FIXME: hack.
        minres = xres `min` yres `min` zres

        -- (3) & (4) : get and tesselate loops
        sqTris :: [TriSquare]
        sqTris = parallelize (nx + ny + nz) $ do
          let segsX = mkSegs _x _yz
              segsY = mkSegs _y _xz
              segsZ = mkSegs _z _xy
          forXYZM (steps - 1) $ \v -> do
            foldMap (tesselateLoop minres obj) $ fromMaybe (error "unclosed loop in paths given") $ getLoops $
              mconcat
                [        segsX v
                , mapR $ segsX (v & _x +~ 1)
                , mapR $ segsY v
                ,        segsY (v & _y +~ 1)
                ,        segsZ v
                , mapR $ segsZ (v & _z +~ 1)
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

------------------------------------------------------------------------------
-- | Run a function lifting every point in a 'Polyline' into R3.
expandPolyline :: (ℝ2 -> ℝ3) -> Polyline -> [ℝ3]
expandPolyline f = fmap f . getPolyline
{-# INLINE expandPolyline #-}

($*) :: Obj2 -> ℝ -> ℝ -> ℝ
f $* a = f . V2 a
infixr 0 $*

(*$) :: Obj2 -> ℝ -> ℝ -> ℝ
f *$ b = \a -> f (V2 a b)
infixr 0 *$

mapR :: [[ℝ3]] -> [[ℝ3]]
mapR = fmap reverse
{-# INLINE mapR #-}


------------------------------------------------------------------------------
-- | Monoidally combine a value produced at every point in the given space.
forXYZM
    :: Monoid m
    => V3 Int         -- ^ Inclusive bounds
    -> (V3 Int -> m)  -- ^ Function to produce monoidal values
    -> m
forXYZM (V3 x y z) f = fold $ do
  zm <- [0 .. z]
  ym <- [0 .. y]
  xm <- [0 .. x]
  pure $ f $ V3 xm ym zm
{-# INLINABLE forXYZM #-}
{-# SPECIALIZE forXYZM :: V3 Int -> (V3 Int -> [TriSquare]) -> [TriSquare] #-}


-- | performance tuning.
-- FIXME: magic number.
forcesteps :: Int
forcesteps = 32
{-# INLINABLE forcesteps #-}


------------------------------------------------------------------------------
-- | Parallelize a list computation by buffering it with @forcesteps@ sparks.
parallelize :: NFData a => Int -> [a] -> [a]
parallelize n x = x `using` parBuffer (max 1 $ div n forcesteps) rdeepseq
{-# INLINABLE parallelize #-}

