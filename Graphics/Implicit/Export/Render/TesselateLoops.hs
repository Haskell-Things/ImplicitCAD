-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop) where

import Prelude((-), pure, ($), length, (==), zip, init, tail, reverse, (<), (/), null, foldl1, (<>), head, (*), abs, (>), (&&), (+), foldMap)

import Graphics.Implicit.Definitions (ℝ, ℕ, Obj3, ℝ3, TriangleMesh(TriangleMesh), Triangle(Triangle))

import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris))

import Graphics.Implicit.Export.Util (centroid)

import Data.List (genericLength)
import Linear ( cross, Metric(norm), (^*), (^/) )

-- de-compose a loop into a series of triangles or squares.
-- FIXME: res should be ℝ3.
tesselateLoop :: ℝ -> Obj3 -> [[ℝ3]] -> [TriSquare]

tesselateLoop _ _ [] = []

-- Given our invariant that the list has already been loopified via
-- 'Graphics.Implicit.Export.Render.GetLoops.getLoops', we know that in the
-- following case, @a = _a@, @b = _b@ and @c = _c@, so we can ignore the
-- duplicated points and just build a triangle.
tesselateLoop _ _ [[a, b], [_b, c], [_c, _a]] = [Tris $ TriangleMesh [Triangle (a,b,c)]]


{-
   #____#     #____#
   |    |     |    |
   #    #  -> #____#
   |    |     |    |
   #____#     #____#
-}

-- NOTE(sandy): in this case, the @[_,_]@s act as bridges between @as@ and
-- @bs@. Thus from our invariant, we know @_a0 = head as@ and @_an = last as@.
-- But I don't see any justification for why it's ok to zip @as@ and @bs@.
-- I think the picture above gives a misleading idea; what if the points on the
-- right side were reversed? Then we'd decompose it like this:
--
--   #     #     #     #      #     #
--   | \ / |     | \   |      |   / |
--   #  X  #  -> #__o__# and  #__o__#
--   | / \ |     |   \ |      | /   |
--   #     #     #     #      #     #
--
-- which feels off.
--
-- I think this function should also insist that $b_n - b_{n-1} = a_n - a_{n-1}$.
tesselateLoop res obj [[_bn,_a0], as@(_:_:_:_),[_an,_b0], bs@(_:_:_:_)]
  | length as == length bs =
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
        preNormal = foldl1 (+)
            [ a `cross` b | (a,b) <- zip path (tail path <> [head path]) ]
        preNormalNorm = norm preNormal
        normal = preNormal ^/ preNormalNorm
        deriv = (obj (mid + (normal ^* (res/100)) ) - midval)/res*100
        mid' = mid - normal ^* (midval/deriv)
    in if abs midval > res/50 && preNormalNorm > 0.5 && abs deriv > 0.5
              && abs (midval/deriv) < 2*res && 3*abs (obj mid') < abs midval
        then early_tris <> [Triangle (a,b,mid') | (a,b) <- zip path (tail path <> [head path]) ]
        else early_tris <> [Triangle (a,b,mid) | (a,b) <- zip path (tail path <> [head path]) ]


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
