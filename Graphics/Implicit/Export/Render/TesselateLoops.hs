-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop) where

import Prelude(max, min, otherwise, sum, (-), pure, ($), length, (==), zip, init, reverse, (<), (<=), (/), null, (<>), (*), abs, (+), foldMap, (&&), drop, Int)

import Graphics.Implicit.Definitions (ℝ, ℕ, Obj3, ℝ3, TriangleMesh(TriangleMesh), Triangle(Triangle))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris,Sq))

import Graphics.Implicit.Export.Util (centroid)

import Data.List (genericLength)
import Linear ( cross, dot, normalize, quadrance, Metric(norm), (^*), (^/), V2(V2))

tail :: [a] -> [a]
tail = drop 1

-- de-compose a loop into a series of triangles or squares.
-- FIXME: res should be ℝ3.
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
    foldMap (tesselateLoop res obj)
        [[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
            where pairs = zip (reverse as) bs

tesselateLoop res obj [as@(_:_:_:_),[_,_], bs@(_:_:_:_), [_,_] ] | length as == length bs =
    foldMap (tesselateLoop res obj)
        [[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
            where pairs = zip (reverse as) bs

{-
   #__#
   |  |  -> if we find a parallelogram then construct a quad.
   #__#
-}
tesselateLoop _ _ [[a,_],[b,_],[c,_],[d,_]] | centroid [a,c] ~= centroid [b,d] = [Sq (b1,b2,b3) z xR yR (a,b,c,d)]
  where
    -- Basis vectors.
    b1 = normalize $ a - b
    -- Note: We re-reflect B2 against B3 here to ensure it's perpendicular to B1. This is to encourage matches, and work around floating point error.
    b2 = normalize $ b3u `cross` b1
    b3u = normalize $ b1 `cross` b2r
    -- The un-reflected b2
    b2r = c - b
    b3 = normalize $ b1 `cross` b2
    -- Z height
    z = a `dot` b3
    -- Ranges of surface covered by square
    xR = V2 (min x1 x2) (max x1 x2)
    yR = V2 (min y1 y2) (max y1 y2)
    x1 = a `dot` b1
    x2 = c `dot` b1
    y1 = a `dot` b2
    y2 = c `dot` b2
    -- Equivalency checking for our center position of the two lines segments crossing the (hopefully) parallelogram.
    (~=) u v = quadrance (u - v) <= eps
    -- Our fudge factor.
    eps :: ℝ
    eps = 1e-8

{-
   #__#      #__#
   |  |  ->  | /|
   #__#      #/_#
-}
-- | Create a pair of triangles from a quad.
-- FIXME: magic number
tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | obj (centroid [a,c]) < res/30 =
    pure $ Tris $ TriangleMesh [Triangle (a,b,c), Triangle (a,c,d)]

-- Fallback case: make fans

-- FIXME: magic numbers.
tesselateLoop res obj pathSides = pure $ Tris $ TriangleMesh $
    let
        path' = foldMap init pathSides
        (early_tris,path) = shrinkLoop 0 path' res obj
    in if null path
    then early_tris
    else let
        mid = centroid path
        midval = obj mid
        rotateList :: Int -> [a] -> [a]
        rotateList 0 l = l
        rotateList _ [] = []
        rotateList _ [a] = [a]
        rotateList n (a:as) = rotateList (n-1) (as <> [a])
        preNormal = sum
            [ a `cross` b | (a,b) <- zip path (rotateList 1 path) ]
        preNormalNorm = norm preNormal
        normal = preNormal ^/ preNormalNorm
        deriv = (obj (mid + (normal ^* (res/100)) ) - midval)/res*100
        mid' = mid - normal ^* (midval/deriv)
        midval' = obj mid'
        isCloserToSurface = abs midval' < abs midval
        isNearby = norm (mid - mid') < 2 * abs midval
    in if isCloserToSurface && isNearby
        then early_tris <> [Triangle (a,b,mid') | (a,b) <- zip path (rotateList 1 path) ]
        else early_tris <> [Triangle (a,b,mid) | (a,b) <- zip path (rotateList 1 path) ]

shrinkLoop :: ℕ -> [ℝ3] -> ℝ -> Obj3 -> ([Triangle], [ℝ3])

shrinkLoop _ path@[a,b,c] res obj =
    if   abs (obj $ centroid [a,b,c]) < res/50
    then
        ( [Triangle (a,b,c)], [])
    else
        ([], path)

-- FIXME: magic number.
shrinkLoop n path@(a:b:c:xs) res obj | n < genericLength path =
    if abs (obj (centroid [a,c])) < res/50
    then
        let (tris,remainder) = shrinkLoop 0 (a:c:xs) res obj
        in (Triangle (a,b,c):tris, remainder)
    else
        shrinkLoop (n+1) (b:c:xs <> [a]) res obj

shrinkLoop _ path _ _ = ([],path)
