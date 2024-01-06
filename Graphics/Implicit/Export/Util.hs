-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- For the definition of centroid.
{-# LANGUAGE FlexibleContexts #-}

-- Functions to make meshes/polylines finer.

module Graphics.Implicit.Export.Util (normTriangle, normVertex, centroid) where

import Prelude(Num, Applicative, Foldable, pure, (+), Fractional, (/), (-), realToFrac, length)

import Graphics.Implicit.Definitions (ℝ, ℝ3, Obj3, Triangle(Triangle), NormedTriangle(NormedTriangle))
import Linear ((*^), (^/), normalize, V3(V3))
import Data.List (foldl')

-- Change the default for bare numbers in this file.
default (ℝ)

-- FIXME: magic numbers.
normTriangle :: ℝ -> Obj3 -> Triangle -> NormedTriangle
normTriangle res obj (Triangle (a,b,c)) =
    NormedTriangle ((a, normify a'), (b, normify b'), (c, normify c'))
        where
            normify = normVertex res obj
            a' = (a + r*^b + r*^c) ^/ 1.02
            b' = (b + r*^a + r*^c) ^/ 1.02
            c' = (c + r*^b + r*^a) ^/ 1.02
            r :: ℝ
            r = 0.01

-- FIXME: magic numbers.
normVertex :: ℝ -> Obj3 -> ℝ3 -> ℝ3
normVertex res obj p =
    let
        -- D_vf(p) = ( f(p) - f(p+v) ) /|v|
        -- but we'll actually scale v by res, so then |v| = res
        -- and that f is obj
        -- and is fixed at p
        -- so actually: d v = ...
        d :: ℝ3 -> ℝ
        d v = ( obj (p + (res/100)*^v) - obj (p - (res/100)*^v) ) / (res/50)
        dx = d (V3 1 0 0)
        dy = d (V3 0 1 0)
        dz = d (V3 0 0 1)
    in normalize (V3 dx dy dz)

-- Get a centroid of a series of points.
centroid :: (Fractional a, Foldable t, Applicative f, Num (f a)) => t (f a) -> f a
centroid pts = foldl' (+) (pure 0) pts ^/ realToFrac (length pts)
{-# INLINABLE centroid #-}

