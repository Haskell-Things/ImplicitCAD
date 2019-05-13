-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to derive FastN when declaring fastℕ.
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE StandaloneDeriving #-}


module Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ), toFastℕ, fromFastℕ) where

import Prelude (Integral, Num, Eq, Ord, Enum, Real, Show, ($), Read, Int)

import qualified Prelude as P ((+), (*), abs, negate, signum, fromInteger, succ, pred, toEnum, fromEnum, quot, rem, quotRem, toInteger, toRational)

import GHC.Real (Ratio((:%)))

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  toFastℕ a = Fastℕ a

instance FastN Fastℕ where
  fromFastℕ (Fastℕ a) = Fastℕ a
  toFastℕ a = a

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
newtype Fastℕ = Fastℕ Int
  deriving (Show, Read, Eq, Ord)

instance Real Fastℕ where
  toRational (Fastℕ a) = P.toInteger a :% 1

fastℕBoth :: (Int, Int) -> (Fastℕ, Fastℕ)
fastℕBoth (a, b) = (Fastℕ a, Fastℕ b)

instance Integral Fastℕ where
  toInteger (Fastℕ a)      = P.toInteger a
  quotRem (Fastℕ a) (Fastℕ b) = fastℕBoth $ P.quotRem a b
  quot (Fastℕ a) (Fastℕ b) = Fastℕ $ P.quot a b
  rem (Fastℕ a) (Fastℕ b)  = Fastℕ $ P.rem a b
  

instance Num Fastℕ where
  (+) (Fastℕ a) (Fastℕ b) = Fastℕ $ a P.+ b
  (*) (Fastℕ a) (Fastℕ b) = Fastℕ $ a P.* b
  abs (Fastℕ a) = Fastℕ $ P.abs a
  negate (Fastℕ a) = Fastℕ $ P.negate a
  signum (Fastℕ a) = Fastℕ $ P.signum a
  fromInteger a = Fastℕ $ P.fromInteger a

instance Enum Fastℕ where
  succ (Fastℕ x) = Fastℕ $ P.succ x
  pred (Fastℕ x) = Fastℕ $ P.pred x
  toEnum n = Fastℕ $ P.toEnum n
  fromEnum (Fastℕ n) = n

