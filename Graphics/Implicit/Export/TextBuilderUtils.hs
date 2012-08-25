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
    ,buildFloat
    -- Serialize a float with four decimal places
    ,buildTruncFloat
    -- Values from Data.Monoid
    ,(<>)
    ,mconcat
    ,mempty
     
                                                 ) where
import Data.Text.Lazy
import Data.Monoid

import Data.Text.Lazy
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int

import Graphics.Implicit.Definitions

import Prelude hiding (replicate)

buildFloat, buildTruncFloat :: ℝ -> Builder

buildFloat = formatRealFloat Fixed Nothing

buildTruncFloat = formatRealFloat Fixed $ Just 4

buildInt :: Int -> Builder
buildInt = decimal