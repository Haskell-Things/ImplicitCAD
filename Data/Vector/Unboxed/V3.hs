{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Vector.Unboxed.V3
  ( VectorV3 ()
  , mkVectorV3
  , (!)
  , memoize
  ) where

import qualified Data.Vector.Unboxed as V
import           Linear ( V3(V3) )
import           Prelude (Int, Double, mod, div, Eq, Ord, Show, ($), (*), (+))


------------------------------------------------------------------------------
-- | An extremely fast, dense map from -- @'V3' Int@ to @a@. Under the hood,
-- this is implemented as 'Data.Vector.Unboxed.Vector', laid out in memory to
-- ensure locality of the Z and then Y dimensions.
data VectorV3 a = VectorV3
  { vv3Bounds   :: {-# UNPACK #-} !(V3 Int)
  , vv3Stride   :: {-# UNPACK #-} !Int
    -- ^ The stride between @V3 x y z@ and @V3 x y (z + 1)@
  , vv3Contents :: !(V.Vector a)
  } deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
-- | Construct a 'VectorV3' by running the given function for every point from
-- @'V3' 0 0 0@ to @bounds@ (inclusive.)
--
-- /O(xyz)/ for bounds @'V3' x y z@
mkVectorV3
    :: V.Unbox a
    => V3 Int         -- ^ The inclusive bounds of the largest element to be stored in the 'VectorV3''.
    -> (V3 Int -> a)  -- ^ The generation function
    -> VectorV3 a
-- We add one to the bounds here because vectors are exclusive by default, but
-- we'd like to be inclusive to support the use case of
-- 'Graphics.Implicit.Export.Render.getMesh'.
mkVectorV3 ((+1) -> bounds@(V3 x y z)) f =
  VectorV3 (bounds) (x * y) $
    V.generate (x * y * z) $ \ix ->
      f $ unindex (V3 x y z) ix
{-# INLINE mkVectorV3 #-}
{-# SPECIALIZE mkVectorV3 :: V3 Int -> (V3 Int -> Double) -> VectorV3 Double #-}


------------------------------------------------------------------------------
-- | Memoize a function by transforming it into a 'VectorV3'-backed lookup.
--
-- Provides amortized /O(1)/ lookups to the given function
memoize :: V.Unbox a => V3 Int -> (V3 Int -> a) -> V3 Int -> a
memoize bounds f =
  let vv3 = mkVectorV3 bounds f
   in \ix -> vv3 ! ix
{-# INLINE memoize #-}
{-# SPECIALIZE memoize :: V3 Int -> (V3 Int -> Double) -> V3 Int -> Double #-}


------------------------------------------------------------------------------
-- | Lookup an element in the map.
--
-- /O(1)/
(!) :: V.Unbox a => VectorV3 a -> V3 Int -> a
(!) vv3 =
  let ix = getIndex vv3
   in \v' -> vv3Contents vv3 V.! ix v'
{-# INLINE (!) #-}
{-# SPECIALIZE (!) :: VectorV3 Double -> V3 Int -> Double #-}


------------------------------------------------------------------------------
-- | Get the internal vector index for a point in V3 space.
--
-- /O(1)/
getIndex
      :: VectorV3 a
      -> V3 Int      -- ^ v3 index
      -> Int         -- ^ vector index
getIndex vv3 =
  let (V3 nx _ _) = vv3Bounds vv3
      stride = vv3Stride vv3
   in \(V3 ix iy iz) -> iz * stride + iy * nx + ix
{-# INLINE getIndex #-}


------------------------------------------------------------------------------
-- | Get a V3 point from the internal vector index.
--
-- /O(1)/
unindex
    :: V3 Int   -- ^ bounds
    -> Int      -- ^ vector index
    -> V3 Int   -- ^ V3 index
unindex (V3 nx ny _nz) ix =
  let x = ix `mod` nx
      y = (ix `div` nx) `mod` ny
      z = ix `div` (nx * ny)
   in V3 x y z
{-# INLINE unindex #-}

