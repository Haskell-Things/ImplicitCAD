-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, 2017, 2018, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Graphics.Implicit.IntegralUtil (ℕ, Fastℕ, toℕ, fromℕ, toFastℕ, fromFastℕ) where

import Prelude (Integral(quotRem, toInteger), Num((*),(+),abs,negate,signum,fromInteger), Eq, Ord, Enum(succ, pred, toEnum, fromEnum), Real(toRational), Show, ($), Read, fromIntegral, Int)

import GHC.Integer (timesInteger, plusInteger, absInteger, negateInteger, signumInteger, quotRemInteger, Integer)

import GHC.Real (Ratio((:%)))

default ()

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

-- manual derivations of Integral and Num, so that we do not define unneeded functions.
instance Integral ℕ where
  quotRem (ℕ a) (ℕ b) =
    let (# q, r #) = quotRemInteger a b
    in (ℕ q , ℕ r)
  toInteger (ℕ a) = a

instance Num ℕ where
  (*) a b = ℕ $ timesInteger (fromℕ a) (fromℕ b)
  (+) a b = ℕ $ plusInteger (fromℕ a) (fromℕ b)
  abs a = ℕ $ absInteger (fromℕ a)
  negate a = ℕ $ negateInteger (fromℕ a)
  signum a = ℕ $ signumInteger (fromℕ a)
  fromInteger a = ℕ a

instance Enum ℕ where
  succ (ℕ x) = ℕ $ succ x
  pred (ℕ x) = ℕ $ pred x
  toEnum n = ℕ $ toEnum n
  fromEnum (ℕ n) = fromEnum n
  
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

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  toFastℕ a = Fastℕ a

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
newtype Fastℕ = Fastℕ Int
  deriving (Show, Read, Eq, Ord, FastN, Real, Integral)
--  deriving stock (Show, Read, Eq, Ord)
--  deriving newtype (FastN, Enum, Real, Integral)

instance Num Fastℕ where
  (+) (Fastℕ a) (Fastℕ b) = Fastℕ $ a + b
  (*) (Fastℕ a) (Fastℕ b) = Fastℕ $ a * b
  abs (Fastℕ a) = Fastℕ $ abs a
  negate (Fastℕ a) = Fastℕ $ negate a
  signum (Fastℕ a) = Fastℕ $ signum a
  fromInteger a = Fastℕ $ fromInteger a

instance Enum Fastℕ where
  succ (Fastℕ x) = Fastℕ $ succ x
  pred (Fastℕ x) = Fastℕ $ pred x
  toEnum n = Fastℕ $ toEnum n
  fromEnum (Fastℕ n) = n
