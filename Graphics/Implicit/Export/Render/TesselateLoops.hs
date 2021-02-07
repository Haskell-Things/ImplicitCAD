-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop) where

import Prelude(sum, (-), pure, ($), length, (==), zip, init, tail, reverse, (<), (/), null, (<>), head, (*), abs, (>), (&&), (+), foldMap)

import Graphics.Implicit.Definitions (ℝ, ℕ, Obj3, ℝ3, TriangleMesh(TriangleMesh), Triangle(Triangle), AnnotatedTriangleMesh(AnnotatedTriangleMesh), TriangleProvenance(..))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris), AnnotatedTriSquare(AnnotatedTris))

import Graphics.Implicit.Export.Util (centroid)

import Data.List (genericLength)
import Linear ( cross, Metric(norm), (^*), (^/) )

-- de-compose a loop into a series of triangles or squares.
-- FIXME: res should be ℝ3.
tesselateLoop :: ℝ -> Obj3 -> [[ℝ3]] -> [AnnotatedTriSquare TriangleProvenance]

tesselateLoop _ _ [] = []

tesselateLoop _ _ [[a,b],[_,c],[_,_]] = [AnnotatedTris $ AnnotatedTriangleMesh [(Triangle (a,b,c), TriangleProvenance_TesselateLoop 0)]]


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
   |  |  -> if parallegram then quad
   #__#
-}

-- FIXME: this function is definately broken, resulting in floating squares. see https://github.com/colah/ImplicitCAD/issues/98

{-
tesselateLoop _ _ [[a,_],[b,_],[c,_],[d,_]] | centroid [a,c] == centroid [b,d] =
    let
        b1 = normalized $ a - b
        b2 = normalized $ c - b
        b3 = b1 `cross3` b2
    in [Sq (b1,b2,b3) (a ⋅ b3) (a ⋅ b1, c ⋅ b1) (a ⋅ b2, c ⋅ b2) ]
-}

{-
   #__#      #__#
   |  |  ->  | /|
   #__#      #/_#
-}
-- | Create a pair of triangles from a quad.
-- FIXME: magic number
tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | obj (centroid [a,c]) < res/30 =
    pure $ AnnotatedTris $ AnnotatedTriangleMesh [(Triangle (a,b,c), TriangleProvenance_TesselateLoop 1), (Triangle (a,c,d), TriangleProvenance_TesselateLoop 2)]

-- Fallback case: make fans

-- FIXME: magic numbers.
tesselateLoop res obj pathSides = pure $ AnnotatedTris $ AnnotatedTriangleMesh $
    let
        path' = foldMap init pathSides
        (early_tris,path) = shrinkLoop 0 path' res obj
    in if null path
    then early_tris
    else let
        mid = centroid path
        midval = obj mid
        preNormal = sum
            [ a `cross` b | (a,b) <- zip path (tail path <> [head path]) ]
        preNormalNorm = norm preNormal
        normal = preNormal ^/ preNormalNorm
        deriv = (obj (mid + (normal ^* (res/100)) ) - midval)/res*100
        mid' = mid - normal ^* (midval/deriv)
        midval' = obj mid'
    in if abs midval' < abs midval
        then early_tris <> [(Triangle (a,b,mid'), TriangleProvenance_TesselateLoop 3) | (a,b) <- zip path (tail path <> [head path]) ]
        else early_tris <> [(Triangle (a,b,mid), TriangleProvenance_TesselateLoop 4) | (a,b) <- zip path (tail path <> [head path]) ]


shrinkLoop :: ℕ -> [ℝ3] -> ℝ -> Obj3 -> ([(Triangle, TriangleProvenance)], [ℝ3])

shrinkLoop _ path@[a,b,c] res obj =
    if   abs (obj $ centroid [a,b,c]) < res/50
    then
        ( [(Triangle (a,b,c), TriangleProvenance_TesselateLoop 5)], [])
    else
        ([], path)

-- FIXME: magic number.
shrinkLoop n path@(a:b:c:xs) res obj | n < genericLength path =
    if abs (obj (centroid [a,c])) < res/50
    then
        let (tris,remainder) = shrinkLoop 0 (a:c:xs) res obj
        in ((Triangle (a,b,c), TriangleProvenance_TesselateLoop 6):tris, remainder)
    else
        shrinkLoop (n+1) (b:c:xs <> [a]) res obj

shrinkLoop _ path _ _ = ([],path)
