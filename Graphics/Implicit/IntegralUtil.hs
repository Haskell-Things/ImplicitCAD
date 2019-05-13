-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Allow us to derive N when declaring ℕ.
{-# LANGUAGE DeriveAnyClass #-}


module Graphics.Implicit.IntegralUtil (ℕ, toℕ, fromℕ) where

import Prelude (Integral(toInteger), Num, Eq, Ord, Enum(fromEnum), Real(toRational), Show, ($), Read, fromIntegral, Int, Integer)

import qualified Prelude as P ((+), (*), abs, negate, signum, fromInteger, succ, pred, toEnum, fromEnum, quot, rem, toInteger)

import GHC.Real (Ratio((:%)))

-- So we can produce an instance of Fastℕ  for ℕ.
import Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ))

-- the N typeclass. only used to define the ℕ type, with a few extensions.
class (Integral n) => N n where
  fromℕ :: ℕ -> n
  toℕ :: n -> ℕ

instance N Integer where
  fromℕ (ℕ a) = a
  toℕ a = ℕ a

instance N Fastℕ where
  fromℕ (ℕ a) = Fastℕ $ fromIntegral a
  toℕ a = ℕ $ fromIntegral a

instance N Int where
  fromℕ (ℕ a) = fromIntegral a
  toℕ a = ℕ $ fromIntegral a

-- Arbitrary precision integers. To be used for anything countable, or in ratios.
newtype ℕ = ℕ Integer
  deriving (Show, Read, Eq, Ord, N)
--  deriving stock (Show, Read, Eq, Ord)
--  deriving newtype (N, Real, Enum) 

instance Integral ℕ where
  toInteger (ℕ a)  = a
  quot (ℕ a) (ℕ b) = ℕ $ P.quot a b
  rem (ℕ a) (ℕ b)  = ℕ $ P.rem a b

instance Num ℕ where
  (+) (ℕ a) (ℕ b) = ℕ $ a P.+ b
  (*) (ℕ a) (ℕ b) = ℕ $ a P.* b
  abs (ℕ a) = ℕ $ P.abs a
  negate (ℕ a) = ℕ $ P.negate a
  signum (ℕ a) = ℕ $ P.signum a
  fromInteger a = ℕ a

instance Enum ℕ where
  succ (ℕ x) = ℕ $ P.succ x
  pred (ℕ x) = ℕ $ P.pred x
  toEnum n = ℕ $ P.toEnum n
  fromEnum (ℕ n) = P.fromEnum n
  
--  {-# INLINE enumFrom #-}
--  {-# INLINE enumFromThen #-}
--  {-# INLINE enumFromTo #-}
--  {-# INLINE enumFromThenTo #-}
--  enumFrom x             = enumDeltaInteger   x 1
--  enumFromThen x y       = enumDeltaInteger   x (y-x)
--  enumFromTo x lim       = enumDeltaToInteger x 1     lim
--  enumFromThenTo x y lim = enumDeltaToInteger x (y-x) lim

instance Real ℕ where
  toRational (ℕ a) = a :% 1




