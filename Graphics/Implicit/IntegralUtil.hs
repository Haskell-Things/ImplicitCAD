-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.IntegralUtil (ℕ, toℕ, fromℕ) where

import Prelude (Integral(toInteger, quot, rem, quotRem, div, mod, divMod), Num((+), (*), abs, negate, signum, fromInteger), Eq, Ord, Enum(succ, pred, toEnum, fromEnum), Real(toRational), Show, ($), Read, fromIntegral, Int, Integer)

import qualified Prelude as P ((+), (*), abs, negate, signum, succ, pred, toEnum, fromEnum, quotRem, divMod)

import GHC.Real (Ratio((:%)))

-- So we can produce an instance of Fastℕ for ℕ.
import Graphics.Implicit.FastIntUtil (Fastℕ(Fastℕ))

-- the N typeclass. only used to define the ℕ type.
class (Integral n) => N n where
  fromℕ :: ℕ -> n
  toℕ :: n -> ℕ

instance N Integer where
  fromℕ (ℕ a) = a
  {-# INLINABLE fromℕ #-}
  toℕ a = ℕ a
  {-# INLINABLE toℕ #-}

instance N Fastℕ where
  fromℕ (ℕ a) = Fastℕ $ fromIntegral a
  {-# INLINABLE fromℕ #-}
  toℕ a = ℕ $ fromIntegral a
  {-# INLINABLE toℕ #-}

instance N Int where
  fromℕ (ℕ a) = fromIntegral a
  {-# INLINABLE fromℕ #-}
  toℕ a = ℕ $ fromIntegral a
  {-# INLINABLE toℕ #-}

-- Arbitrary precision integers. To be used for anything countable, or in ratios.
newtype ℕ = ℕ Integer
  deriving (Show, Read, Eq, Ord)

instance Real ℕ where
  toRational (ℕ a) = a :% 1
  {-# INLINABLE toRational #-}

bothℕ :: (Integer, Integer) -> (ℕ, ℕ)
bothℕ (a, b) = (ℕ a , ℕ b)

instance Integral ℕ where
  toInteger (ℕ a)     = a
  {-# INLINABLE toInteger #-}
  quot (ℕ n) (ℕ d)    = ℕ $ q where (q,_) = quotRem n d
  {-# INLINABLE quot #-}
  rem (ℕ n) (ℕ d)     = ℕ $ r where (_,r) = quotRem n d
  {-# INLINABLE rem #-}
  quotRem (ℕ a) (ℕ b) = bothℕ $ P.quotRem a b 
  {-# INLINABLE quotRem #-}
  div (ℕ n) (ℕ d)     = ℕ $ q where (q,_) = divMod n d
  {-# INLINABLE div #-}
  mod (ℕ n) (ℕ d)     = ℕ $ r where (_,r) = divMod n d
  {-# INLINABLE mod #-}
  divMod (ℕ n) (ℕ d)  = bothℕ $ P.divMod n d
  {-# INLINABLE divMod #-}

instance Num ℕ where
  (+) (ℕ a) (ℕ b) = ℕ $ a P.+ b
  {-# INLINABLE (+) #-}
  (*) (ℕ a) (ℕ b) = ℕ $ a P.* b
  {-# INLINABLE (*) #-}
  abs (ℕ a)       = ℕ $ P.abs a
  {-# INLINABLE abs #-}
  negate (ℕ a)    = ℕ $ P.negate a
  {-# INLINABLE negate #-}
  signum (ℕ a)    = ℕ $ P.signum a
  {-# INLINABLE signum #-}
  fromInteger a   = ℕ a
  {-# INLINABLE fromInteger #-}

-- | Note that we do not implement all of the members of the typeclass here.
instance Enum ℕ where
  succ (ℕ x) = ℕ $ P.succ x
  {-# INLINABLE succ #-}
  pred (ℕ x) = ℕ $ P.pred x
  {-# INLINABLE pred #-}
  toEnum n = ℕ $ P.toEnum n
  {-# INLINABLE toEnum #-}
  fromEnum (ℕ n) = P.fromEnum n
  {-# INLINABLE fromEnum #-}
  




