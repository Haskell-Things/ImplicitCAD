-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- For the definition of centroid.
{-# LANGUAGE FlexibleContexts #-}

-- Functions to make meshes/polylines finer.

module Graphics.Implicit.Export.Util (normTriangle, normVertex, centroid) where

import Prelude(Fractional, (/), (-), foldl, realToFrac, length)

import Graphics.Implicit.Definitions (ℝ, ℝ3, Obj3, Triangle(Triangle), NormedTriangle(NormedTriangle))

import Data.VectorSpace (VectorSpace, Scalar, (^+^), (*^), (^/), (^-^), normalized, zeroV)

-- | Change the default for bare numbers in this file.
default (ℝ)

-- FIXME: magic numbers.
normTriangle :: ℝ -> Obj3 -> Triangle -> NormedTriangle
normTriangle res obj (Triangle (a,b,c)) =
    NormedTriangle ((a, normify a'), (b, normify b'), (c, normify c'))
        where
            normify = normVertex res obj
            a' = (a ^+^ r*^b ^+^ r*^c) ^/ 1.02
            b' = (b ^+^ r*^a ^+^ r*^c) ^/ 1.02
            c' = (c ^+^ r*^b ^+^ r*^a) ^/ 1.02
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
        d v = ( obj (p ^+^ (res/100)*^v) - obj (p ^-^ (res/100)*^v) ) / (res/50)
        dx = d (1, 0, 0)
        dy = d (0, 1, 0)
        dz = d (0, 0, 1)
    in normalized (dx,dy,dz)

-- Get a centroid of a series of points.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [v] -> v
centroid pts = foldl (^+^) zeroV pts ^/ realToFrac (length pts)
{-# INLINABLE centroid #-}

{-

-- If we need to make a 2D mesh finer...
divideMesh2To :: ℝ -> [(ℝ2, ℝ2, ℝ2)] -> [(ℝ2, ℝ2, ℝ2)]
divideMesh2To res mesh =
    let
        av :: ℝ2 -> ℝ2 -> ℝ2
        av a b = (a S.+ b) S./ (2.0 :: ℝ)
        divideTriangle :: (ℝ2, ℝ2, ℝ2) -> [(ℝ2, ℝ2, ℝ2)]
        divideTriangle (a,b,c) =
            case (S.norm (a S.- b) > res, S.norm (b S.- c) > res, S.norm (c S.- a) > res) of
                (False, False, False) -> [(a,b,c)]
                (True,  False, False) -> [(a, av a b, c),
                                          (av a b, b, c) ]
                (True,  True,  False) -> [(a, av a b, av a c),
                                              (av a b, b, av a c),
                                          (b, c, av a c)]
                (True,  True,  True ) -> [(a, av a b, av a c),
                                          (b, av b c, av b a),
                                          (c, av c a, av c b),
                                          (av b c, av a c, av a b)]
                (_,_,_) -> divideTriangle (c, a, b)
    in
        foldMap divideTriangle mesh

divideMeshTo :: ℝ -> [(ℝ3, ℝ3, ℝ3)] -> [(ℝ3, ℝ3, ℝ3)]
divideMeshTo res mesh =
    let
        av :: ℝ3 -> ℝ3 -> ℝ3
        av a b = (a S.+ b) S./ (2.0 :: ℝ)
        divideTriangle :: (ℝ3, ℝ3, ℝ3) -> [(ℝ3, ℝ3, ℝ3)]
        divideTriangle (a,b,c) =
            case (S.norm (a S.- b) > res, S.norm (b S.- c) > res, S.norm (c S.- a) > res) of
                (False, False, False) -> [(a,b,c)]
                (True,  False, False) -> [(a, av a b, c),
                                          (av a b, b, c) ]
                (True,  True,  False) -> [(a, av a b, av a c),
                                              (av a b, b, av a c),
                                          (b, c, av a c)]
                (True,  True,  True ) -> [(a, av a b, av a c),
                                          (b, av b c, av b a),
                                          (c, av c a, av c b),
                                          (av b c, av a c, av a b)]
                (_,_,_) -> divideTriangle (c, a, b)
    in
        foldMap divideTriangle mesh

dividePolylineTo :: ℝ -> [ℝ2] -> [ℝ2]
dividePolylineTo res polyline =
    let
        av :: ℝ2 -> ℝ2 -> ℝ2
        av a b = (a S.+ b) S./ (2.0 :: ℝ)
        divide a b =
            if S.norm (a S.- b) <= res
            then [a]
            else divide a (av a b) <> divide (av a b) b
        n = length polyline
    in do
        m <- [0.. n]
        if m /= n
            then divide (polyline !! m) (polyline !! (m+1))
            else [polyline !! n]
-}
