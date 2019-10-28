-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Use existing instances for the wrapped types rather than manually manking them
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ), toFastℕ, fromFastℕ) where

import Prelude (Integral, Num, Eq, Ord, Enum, Real, Show, Read, Int, id)

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  {-# INLINABLE fromFastℕ #-}
  toFastℕ = Fastℕ
  {-# INLINABLE toFastℕ #-}

instance FastN Fastℕ where
  fromFastℕ = id
  {-# INLINABLE fromFastℕ #-}
  toFastℕ = id
  {-# INLINABLE toFastℕ #-}

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
-- When Read and Show instances exist on a given type they need to satisfy
-- read . show = id
newtype Fastℕ = Fastℕ Int
  deriving (Read, Show, Eq, Ord, Num, Enum, Integral, Real)
