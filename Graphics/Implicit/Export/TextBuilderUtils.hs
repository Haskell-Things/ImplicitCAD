-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This module exists to re-export a coherent set of functions to define
-- Data.Text.Lazy builders with.

module Graphics.Implicit.Export.TextBuilderUtils (
     -- Values from Data.Text.Lazy
     Text,
     pack,
     -- Values from Data.Text.Lazy.Builder, as well as some special builders
     Builder,
     toLazyText,
     fromLazyText,
     buildInt,
     buildℕ,
     -- Serialize a float in full precision
     bf,
     -- Serialize a float with four decimal places
     buildTruncFloat,
     -- Values from Data.Monoid
     (<>),
     mconcat,
     mempty
    ) where

import Prelude (Maybe(Nothing, Just), ($))

import Graphics.Implicit.Definitions(Fastℕ)
import Data.Text.Lazy (Text, pack)
-- We manually redefine this operator to avoid a dependency on base >= 4.5
-- This will become unnecessary later.
import Data.Monoid (Monoid, mappend, mconcat, mempty)

import Data.Text.Internal.Lazy (defaultChunkSize)
import Data.Text.Lazy.Builder (Builder, toLazyTextWith, fromLazyText)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(Exponent, Fixed))
import Data.Text.Lazy.Builder.Int (decimal)

import Graphics.Implicit.Definitions (ℝ, ℕ)

-- The chunk size for toLazyText is very small (128 bytes), so we export
-- a version with a much larger size (~16 K)
toLazyText :: Builder -> Text
toLazyText = toLazyTextWith defaultChunkSize

bf, buildTruncFloat :: ℝ -> Builder

bf = formatRealFloat Exponent Nothing

buildTruncFloat = formatRealFloat Fixed $ Just 4

buildℕ :: ℕ -> Builder
buildℕ = decimal

buildInt :: Fastℕ -> Builder
buildInt = decimal

-- This is directly copied from base 4.5.1.0
infixr 6 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
