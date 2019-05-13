-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE TypeFamilies #-}

module Graphics.Implicit.RationalFunctions (cosℝp, sinℝp, asinℝp, normalizeℝp, normalizeℝ2p, normalizeℝ3p, sqrtℝp, cbrtℝp) where

import Prelude (Double, (+), (*), (<), (^), (>), (-), ($), (/=), abs, otherwise, fromIntegral, sum, realToFrac, Integer, (/), negate, (**))

import qualified Prelude as P (sqrt)

import Data.Maybe(Maybe(Nothing), fromJust)

import Data.List(find, iterate)

import Graphics.Implicit.IntegralUtil (ℕ, toℕ, fromℕ)

import Graphics.Implicit.FastIntUtil (Fastℕ)

import Data.VectorSpace (InnerSpace, Scalar, (^/), magnitudeSq)

import Data.Ratio ((%))

import GHC.Real (Ratio((:%)))

-- FIXME: since the taylor series is acurate only in a certain range, rollover in that range.
asinℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
asinℝp precision x
  | x > 0 = res
  | x < 0 = -res
  | otherwise = 0
    where
      res=((abs x))+(sum [asinTerm (abs x) i | i <- [1..precision]])

asinTerm :: Ratio ℕ -> Fastℕ -> Ratio ℕ
asinTerm x n = ((x^oddTerm)*((factorialskip $ oddTerm-2)%1)) / (((factorialskip evenTerm)*(toℕ $ oddTerm))%1)
  where
    oddTerm :: Fastℕ
    oddTerm = 2*n + 1
    evenTerm :: Fastℕ
    evenTerm = 2*n

factorialskip :: Fastℕ -> ℕ
factorialskip 0 = 1
factorialskip 1 = 1
factorialskip n = (toℕ n) * factorialskip (n-2)

cosℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
cosℝp precision x = 1-(sum [cosTerm (abs x) (toℕ i) | i <- [1..precision]])

cosTerm :: Ratio ℕ -> ℕ -> Ratio ℕ
cosTerm x i = ((x^evenTerm) / ((factorial evenTerm)%1))*(-1)^(i-1)
  where evenTerm = 2*i 

sinℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
sinℝp precision x
  | x > 0 = res
  | x < 0 = -res
  | otherwise = 0
    where
      res=sum [sinTerm (abs x) (toℕ i) | i <- [1..precision]]

sinTerm :: Ratio ℕ -> ℕ -> Ratio ℕ
sinTerm x i = (x^oddTerm / ((factorial oddTerm)%1))*(-1)^(i-1)
  where oddTerm = 2*i - 1

factorial :: ℕ -> ℕ
factorial 1 = 1
factorial n = n * factorial (n-1)

-- | normalize a rational.
normalizeℝp :: (InnerSpace v, s ~ Scalar v, s ~ Ratio ℕ) =>  Fastℕ -> v -> v
normalizeℝp precision v = v ^/ magnitudeForced
  where
    magnitudeForced :: (Ratio ℕ)
    magnitudeForced = sqrtℝp precision $ magnitudeSq v
{-# INLINABLE normalizeℝp #-}

-- | normalize a tuple of two rationals.
normalizeℝ2p :: (InnerSpace s, v ~ Scalar (s,s), s ~ Ratio ℕ) =>  Fastℕ -> (s, s) -> (v, v)
normalizeℝ2p precision v = v ^/ magnitudeForced
  where
    magnitudeForced :: (Ratio ℕ)
    magnitudeForced = sqrtℝp precision $ magnitudeSq v
{-# INLINABLE normalizeℝ2p #-}

-- | normalize a tuple of three rationals.
normalizeℝ3p :: (InnerSpace s, v ~ Scalar (s,s,s), s ~ Ratio ℕ) =>  Fastℕ -> (s, s, s) -> (v, v, v)
normalizeℝ3p precision v = v ^/ magnitudeForced
  where
    magnitudeForced :: (Ratio ℕ)
    magnitudeForced = sqrtℝp precision $ magnitudeSq v
-- | get a cube root using our recursive function, using the double based method as a springboard..
cbrtℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
cbrtℝp precision x
  | x > 0 = fromJust $ refineCbrtGuess precision (abs x) $ preseed x
  | x < 0 = negate $ fromJust $ refineCbrtGuess precision (abs x) $ preseed x
  | otherwise = 0
    where
    preseed :: Ratio ℕ -> Ratio ℕ
    preseed p = realToFrac $ (**(1/3)) $ ratℕ2Double p
    ratℕ2Double :: Ratio ℕ -> Double
    ratℕ2Double ( n :% d ) = (fromIntegral (fromℕ n :: Integer)) / (fromIntegral (fromℕ d :: Integer))

-- | Use newton's method to zoom in on a correct answer.
--   See: https://codereview.stackexchange.com/questions/63743/square-root-calculation-with-newtons-method
refineCbrtGuess :: Fastℕ -> Ratio ℕ -> Ratio ℕ -> Maybe (Ratio ℕ)
refineCbrtGuess precision x initial
  | x /= 0 = find withinPrecision $ iterate newtons initial
  | otherwise = Nothing
  where
    withinPrecision guess = abs (guess*guess*guess - x) < (1%(2^precision))*x
    newtons guess = (2*guess + x / (guess*guess)) /3

-- | get a square root using our recursive function, using the native square root function as a springboard.
sqrtℝp :: Fastℕ -> Ratio ℕ -> Ratio ℕ
sqrtℝp precision x
  | x /= 0 = fromJust $ refineSqrtGuess precision (abs x) $ preseed x
  | otherwise = 0
    where
    preseed :: Ratio ℕ -> Ratio ℕ
    preseed p = realToFrac $ P.sqrt $ ratℕ2Double p
    ratℕ2Double :: Ratio ℕ -> Double
    ratℕ2Double ( n :% d ) = (fromIntegral (fromℕ n :: Integer)) / (fromIntegral (fromℕ d :: Integer))
-- | Use newton's method to zoom in on a correct answer.
--   See: https://codereview.stackexchange.com/questions/63743/square-root-calculation-with-newtons-method
refineSqrtGuess :: Fastℕ -> Ratio ℕ -> Ratio ℕ -> Maybe (Ratio ℕ)
refineSqrtGuess precision x initial
  | x /= 0 = find withinPrecision $ iterate newtons initial
  | otherwise = Nothing
  where
    withinPrecision guess = abs (guess*guess - x) < (1%(2^precision))*x
    newtons guess = (guess + x / guess) / 2
