{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- For when DEBUG is enabled
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Vector.Unboxed.V3
  ( VectorV3 ()
  , mkVectorV3
  , (!)
  ) where

import qualified Data.Vector.Unboxed as V
import           Linear ( V3(V3) )
import           Prelude (pure, Int, mod, div, Eq, Ord, Show, Bool(True, False), ($), (*), (+), show, error, (>), (<>))
import           Test.QuickCheck


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

getIndex
      :: VectorV3 a  -- ^ bounds
      -> V3 Int      -- ^ v3 index
      -> Int         -- ^ vector index
getIndex vv3 =
  let (V3 nx _ _) = vv3Bounds vv3
      mz = vv3Stride vv3
   in \(V3 ix iy iz) -> iz * mz + iy * nx + ix
{-# INLINE getIndex #-}


data VectorV3 a = VectorV3
  { vv3Bounds   :: {-# UNPACK #-} !(V3 Int)
  , vv3Stride   :: {-# UNPACK #-} !Int
  , vv3Contents :: {-# UNPACK #-} !(V.Vector a)
  } deriving (Eq, Ord, Show)


mkVectorV3 :: V.Unbox a => V3 Int -> (V3 Int -> a) -> VectorV3 a
mkVectorV3 ((+1) -> bounds@(V3 x y z)) f =
  VectorV3 (bounds) (x * y) $
    V.generate (x * y * z) $ \ix ->
      f $ unindex (V3 x y z) ix



(!) :: V.Unbox a => VectorV3 a -> V3 Int -> a
(!) vv3 =
  let ix = getIndex vv3
   in \v' ->
#ifdef DEBUG
      case ix v' > ix (vv3Bounds vv3) of
        True -> error $ "out of bounds: " <> show (v', vv3Bounds vv3)
        False ->
#endif
          vv3Contents vv3 V.! ix v'



test :: Property
test = property $ \(Positive (Small nx))
                   (Positive (Small ny))
                   (Positive (Small nz))
                   (applyFun3 -> f :: Int -> Int -> Int -> Int) -> do
  x <- choose (0, nx)
  y <- choose (0, ny)
  z <- choose (0, nz)

  let ix = V3 x y z
      v3 = mkVectorV3 (V3 nx ny nz) $ \(V3 a b c) -> f a b c
  pure $
    counterexample (show (V3 x y z)) $
      (!) v3 ix === f x y z
      -- unindex nx ny nz (getIndex nx ny nz x y z) === (x, y, z)
      -- lookupIndex nx ny nz (mkVectorV3 nx ny nz f) x y z === f x y z




