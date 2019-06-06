-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop) where

import Prelude(return, ($), length, (==), zip, init, tail, reverse, (<), (/), null, foldl1, (++), head, (*), abs, (>), (&&), (+), concatMap)

import Graphics.Implicit.Definitions (ℝ, ℕ, Obj3, ℝ3, TriangleMesh(TriangleMesh), (⋅), Triangle(Triangle))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq))

import Graphics.Implicit.Export.Util (centroid)

import Data.VectorSpace (normalized, (^-^), (^+^), magnitude, (^/), (^*))

import Data.List (genericLength)

import Data.Cross (cross3)

tesselateLoop :: ℝ -> Obj3 -> [[ℝ3]] -> [TriSquare]

tesselateLoop _ _ [] = []

tesselateLoop _ _ [[a,b],[_,c],[_,_]] = [Tris $ TriangleMesh [Triangle (a,b,c)]]


{-
   #____#     #____#
   |    |     |    |
   #    #  -> #____#
   |    |     |    |
   #____#     #____#
-}

tesselateLoop res obj [[_,_], as@(_:_:_:_),[_,_], bs@(_:_:_:_)] | length as == length bs =
    concatMap (tesselateLoop res obj)
        [[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
            where pairs = zip (reverse as) bs

tesselateLoop res obj [as@(_:_:_:_),[_,_], bs@(_:_:_:_), [_,_] ] | length as == length bs =
    concatMap (tesselateLoop res obj)
        [[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
            where pairs = zip (reverse as) bs

{-
   #__#
   |  |  -> if parallegram then quad
   #__#
-}

-- NOTE: colah thought this was broken.

tesselateLoop _ _ [[a,_],[b,_],[c,_],[d,_]] | centroid [a,c] == centroid [b,d] =
    let
        b1 = normalized $ a ^-^ b
        b2 = normalized $ c ^-^ b
        b3 = b1 `cross3` b2
    in [Sq (b1,b2,b3) (a ⋅ b3) (a ⋅ b1, c ⋅ b1) (a ⋅ b2, c ⋅ b2) ]

{-
   #__#      #__#
   |  |  ->  | /|
   #__#      #/_#
-}

tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | obj (centroid [a,c]) < res/30 =
    return $ Tris $ TriangleMesh [Triangle (a,b,c), Triangle (a,c,d)]

-- Fallback case: make fans

-- FIXME: magic numbers.
tesselateLoop res obj pathSides = return $ Tris $ TriangleMesh $
    let
        path' = concatMap init pathSides
        (early_tris,path) = shrinkLoop 0 path' res obj
    in if null path
    then early_tris
    else let
        mid@(_,_,_) = centroid path
        midval = obj mid
        preNormal = foldl1 (^+^)
            [ a `cross3` b | (a,b) <- zip path (tail path ++ [head path]) ]
        preNormalNorm = magnitude preNormal
        normal = preNormal ^/ preNormalNorm
        deriv = (obj (mid ^+^ (normal ^* (res/100)) ) ^-^ midval)/res*100
        mid' = mid ^-^ normal ^* (midval/deriv)
    in if abs midval > res/50 && preNormalNorm > 0.5 && abs deriv > 0.5
              && abs (midval/deriv) < 2*res && 3*abs (obj mid') < abs midval
        then early_tris ++ [Triangle (a,b,mid') | (a,b) <- zip path (tail path ++ [head path]) ]
        else early_tris ++ [Triangle (a,b,mid) | (a,b) <- zip path (tail path ++ [head path]) ]


shrinkLoop :: ℕ -> [ℝ3] -> ℝ -> Obj3 -> ([Triangle], [ℝ3])

shrinkLoop _ path@[a,b,c] res obj =
    if   abs (obj $ centroid [a,b,c]) < res/50
    then
        ( [Triangle (a,b,c)], [])
    else
        ([], path)

shrinkLoop n path@(a:b:c:xs) res obj | n < genericLength path =
    if abs (obj (centroid [a,c])) < res/50
    then
        let (tris,remainder) = shrinkLoop 0 (a:c:xs) res obj
        in ((Triangle (a,b,c)):tris, remainder)
    else
        shrinkLoop (n+1) (b:c:xs ++ [a]) res obj

shrinkLoop _ path _ _ = ([],path)
