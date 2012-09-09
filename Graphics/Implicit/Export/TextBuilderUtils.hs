-- This module exists to re-export a coherent set of functions to define
-- Data.Text.Lazy builders with.


module Graphics.Implicit.Export.TextBuilderUtils  
    (
     -- Values from Data.Text.Lazy
     Text
    ,pack
    ,replicate
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
    ,mconcat
    ,mempty
     
                                                 ) where
import Data.Text.Lazy
-- We manually redefine this operator to avoid a dependency on base >= 4.5
-- This will become unnecessary later.
import Data.Monoid hiding ((<>))

import Data.Text.Lazy
import Data.Text.Lazy.Internal (defaultChunkSize)
import Data.Text.Lazy.Builder hiding (toLazyText)
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int

import Graphics.Implicit.Definitions

import Prelude hiding (replicate)


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
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}