-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ), toFastℕ, fromFastℕ) where

import Prelude (Integral(toInteger, quot, rem, quotRem, div, mod, divMod), Num((+), (*), abs, negate, signum, fromInteger), Eq, Ord, Enum(succ, pred, toEnum, fromEnum), Real(toRational), Show(show), ($), Read, Int)

import qualified Prelude as P ((+), (*), abs, negate, signum, fromInteger, succ, pred, toEnum, quotRem, divMod, toInteger)

import GHC.Real (Ratio((:%)))

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  {-# INLINABLE fromFastℕ #-}
  toFastℕ a = Fastℕ a
  {-# INLINABLE toFastℕ #-}

instance FastN Fastℕ where
  fromFastℕ (Fastℕ a) = Fastℕ a
  {-# INLINABLE fromFastℕ #-}
  toFastℕ a = a
  {-# INLINABLE toFastℕ #-}

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
newtype Fastℕ = Fastℕ Int
  deriving (Read, Eq, Ord)

instance Real Fastℕ where
  toRational (Fastℕ a) = P.toInteger a :% 1
  {-# INLINABLE toRational #-}

instance Show Fastℕ where
  show (Fastℕ n) = show n

fastℕBoth :: (Int, Int) -> (Fastℕ, Fastℕ)
fastℕBoth (a, b) = (Fastℕ a, Fastℕ b)
{-# INLINABLE fastℕBoth #-}

instance Integral Fastℕ where
  toInteger (Fastℕ a)         = P.toInteger a
  {-# INLINABLE toInteger #-}
  quot (Fastℕ n) (Fastℕ d)    = Fastℕ $ q where (q,_) = quotRem n d
  {-# INLINABLE quot #-}
  rem (Fastℕ n) (Fastℕ d)     = Fastℕ $ r where (_,r) = quotRem n d
  {-# INLINABLE rem #-}
  quotRem (Fastℕ a) (Fastℕ b) = fastℕBoth $ P.quotRem a b
  {-# INLINABLE quotRem #-}
  div (Fastℕ n) (Fastℕ d)     = Fastℕ $ q where (q,_) = divMod n d
  {-# INLINABLE div #-}
  mod (Fastℕ n) (Fastℕ d)     = Fastℕ $ r where (_,r) = divMod n d
  {-# INLINABLE mod #-}
  divMod (Fastℕ n) (Fastℕ d)  = fastℕBoth $ P.divMod n d
  {-# INLINABLE divMod #-}

instance Num Fastℕ where
  (+) (Fastℕ a) (Fastℕ b) = Fastℕ $ a P.+ b
  {-# INLINABLE (+) #-}
  (*) (Fastℕ a) (Fastℕ b) = Fastℕ $ a P.* b
  {-# INLINABLE (*) #-}
  abs (Fastℕ a)           = Fastℕ $ P.abs a
  {-# INLINABLE abs #-}
  negate (Fastℕ a)        = Fastℕ $ P.negate a
  {-# INLINABLE negate #-}
  signum (Fastℕ a)        = Fastℕ $ P.signum a
  {-# INLINABLE signum #-}
  fromInteger a           = Fastℕ $ P.fromInteger a
  {-# INLINABLE fromInteger #-}

instance Enum Fastℕ where
  succ (Fastℕ x)     = Fastℕ $ P.succ x
  {-# INLINABLE succ #-}
  pred (Fastℕ x)     = Fastℕ $ P.pred x
  {-# INLINABLE pred #-}
  toEnum           n = Fastℕ $ P.toEnum n
  {-# INLINABLE toEnum #-}
  fromEnum (Fastℕ n) = n
  {-# INLINABLE fromEnum #-}
