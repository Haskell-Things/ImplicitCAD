-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Lift the numeric instances where we can
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Suppress a warning about the derived Integral instance
{-# OPTIONS_GHC -Wno-identities #-}

module Graphics.Implicit.IntegralUtil (ℕ, toℕ, fromℕ) where

import Prelude (Integral, Integer, Int, Show, Read, Eq, Ord, Num, Enum, Integral, Real, ($), fromIntegral, (.))

-- So we can produce an instance of Fastℕ for ℕ.
import Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ))

-- the N typeclass. only used to define the ℕ type.
class (Integral n) => N n where
  fromℕ :: ℕ -> n
  toℕ :: n -> ℕ

instance N Integer where
  fromℕ (ℕ a) = a
  {-# INLINABLE fromℕ #-}
  toℕ = ℕ
  {-# INLINABLE toℕ #-}

instance N Fastℕ where
  fromℕ (ℕ a) = Fastℕ $ fromIntegral a
  {-# INLINABLE fromℕ #-}
  toℕ = ℕ . fromIntegral
  {-# INLINABLE toℕ #-}

instance N Int where
  fromℕ (ℕ a) = fromIntegral a
  {-# INLINABLE fromℕ #-}
  toℕ = ℕ . fromIntegral
  {-# INLINABLE toℕ #-}

-- Arbitrary precision integers. To be used for anything countable, or in ratios.
-- When Read and Show instances exist on a given type they need to satisfy
-- read . show = id
newtype ℕ = ℕ Integer
 deriving (Show, Read, Eq, Ord, Num, Enum, Integral, Real)
