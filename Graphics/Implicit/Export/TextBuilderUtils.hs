-- This module exists to re-export a coherent set of functions to define
-- Data.Text.Lazy builders with.


module Graphics.Implicit.Export.TextBuilderUtils  
    (
     -- Values from Data.Text.Lazy
     Text
    ,pack
    -- Values from Data.Text.Lazy.Builder, as well as some special builders
    ,Builder
    ,toLazyText
    ,fromLazyText
    ,buildInt
    -- Serialize a float in full precision
    ,bf
    -- Serialize a float with four decimal places
    ,buildTruncFloat
    -- Values from Data.Monoid
    ,(<>)
    ,Monoid.mconcat
    ,Monoid.mempty
     
                                                 ) where
import Data.Text.Lazy
-- We manually redefine this operator to avoid a dependency on base >= 4.5
-- This will become unnecessary later.
import qualified Data.Monoid as Monoid

import Data.Text.Lazy.Builder hiding (toLazyText)
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int

import Foreign.Storable (sizeOf)
import Data.Bits (shiftL)

import Graphics.Implicit.Definitions

-- Helper functions from Data.Text.Internal.Lazy.  These functions are needed
-- here instead of imported as the API is not meant to be used in this way and
-- is broken in some version of the Text library.
-- DO NOT EXPORT from this module
defaultChunkSize :: Int
defaultChunkSize = 16384 - chunkOverhead
{-# INLINE defaultChunkSize #-}

-- DO NOT EXPORT from this module
-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = sizeOf (undefined :: Int) `shiftL` 1
{-# INLINE chunkOverhead #-}

-- The chunk size for toLazyText is very small (128 bytes), so we export
-- a version with a much larger size (~16 K)
toLazyText :: Builder -> Text
toLazyText = toLazyTextWith defaultChunkSize

bf, buildTruncFloat :: ℝ -> Builder

bf = formatRealFloat Exponent Nothing
{-# INLINE bf #-}

buildTruncFloat = formatRealFloat Fixed $ Just 4

buildInt :: Int -> Builder
buildInt = decimal

-- This is directly copied from base 4.5.1.0
infixr 6 <>
(<>) :: Monoid.Monoid m => m -> m -> m
(<>) = Monoid.mappend
{-# INLINE (<>) #-}
