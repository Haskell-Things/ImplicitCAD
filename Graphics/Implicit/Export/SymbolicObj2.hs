-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This file symbolicaly renders contours and contour fillings.
-- If it can't, it passes the puck to a marching-squares-like
-- algorithm...

module Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour) where

import Prelude((==), pure, fmap, ($), (-), (/), (+), (>), (*), reverse, cos, pi, sin, max, ceiling, (<$>))

import Graphics.Implicit.Definitions (objectRounding, defaultObjectContext, ObjectContext, ℝ, ℝ2, Fastℕ, SymbolicObj2(Square, Circle, Shared2), SharedObj(Translate, Scale, WithRounding), Polyline(Polyline), Polytri(Polytri), (⋯*), fromFastℕtoℝ)

import Linear ( Metric(norm), V2(V2), (^/) )

import Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh)

import Graphics.Implicit.Export.Render (getContour)
import Graphics.Implicit.Primitives (getImplicit)

symbolicGetOrientedContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetOrientedContour res symbObj = orient <$> symbolicGetContour res defaultObjectContext symbObj
    where
        obj = getImplicit symbObj
        -- FIXME: cowardly case handling.
        orient :: Polyline -> Polyline
        orient (Polyline points@(p1:p2:_)) =
            let
                v = (\(V2 a b) -> V2 b (-a)) (p2 - p1)
                dv = v ^/ (norm v / res / 0.1)
            in if obj (p1 + dv) - obj p1 > 0
            then Polyline points
            else Polyline $ reverse points
        orient (Polyline []) = Polyline []
        orient (Polyline [_]) = Polyline []

symbolicGetContour :: ℝ -> ObjectContext ->  SymbolicObj2 -> [Polyline]
symbolicGetContour _ ctx (Square (V2 dx dy)) | objectRounding ctx == 0 =
  [Polyline [V2 0 0, V2 dx 0, V2 dx dy, V2 0 dy, V2 0 0]]
-- FIXME: magic number.
symbolicGetContour res _ (Circle r) =
  [ Polyline
    [ V2 (r*cos(2*pi*fromFastℕtoℝ m/fromFastℕtoℝ n)) (r*sin(2*pi*fromFastℕtoℝ m/fromFastℕtoℝ n))
    | m <- [0.. n]
    ]
  ]
  where
    n :: Fastℕ
    n = max 5 $ ceiling $ 2*pi*r/res
symbolicGetContour res ctx (Shared2 (WithRounding r obj)) = symbolicGetContour res (ctx { objectRounding = r }) obj
symbolicGetContour res ctx (Shared2 (Translate v obj)) = appOpPolylines (+ v) $ symbolicGetContour res ctx obj
symbolicGetContour res ctx (Shared2 (Scale s@(V2 a b) obj)) = appOpPolylines (⋯* s) $ symbolicGetContour (res/sc) ctx obj
    where sc = max a b
symbolicGetContour res _ obj = getContour (pure res) obj

appOpPolylines :: (ℝ2 -> ℝ2) -> [Polyline] -> [Polyline]
appOpPolylines op = fmap (appOpPolyline op)
appOpPolyline :: (ℝ2 -> ℝ2) -> Polyline -> Polyline
appOpPolyline op (Polyline xs) = Polyline $ fmap op xs

symbolicGetContourMesh :: ℝ ->  SymbolicObj2 -> [Polytri]
symbolicGetContourMesh res (Shared2 (Translate v obj)) = (\(Polytri (a,b,c)) -> Polytri (a + v, b + v, c + v)) <$>
                                                symbolicGetContourMesh res obj
symbolicGetContourMesh res (Shared2 (Scale s@(V2 a b) obj)) = (\(Polytri (c,d,e)) -> Polytri (c ⋯* s, d ⋯* s, e ⋯* s)) <$>
                                                  symbolicGetContourMesh (res/sc) obj where sc = max a b
-- TODO(sandy):  only if rounding = 0
symbolicGetContourMesh _ (Square (V2 dx dy)) = [Polytri (V2 0 0, V2 dx 0, V2 dx dy), Polytri (V2 dx dy, V2 0 dy, V2 0 0) ]
-- FIXME: magic number.
symbolicGetContourMesh res (Circle r) =
    [ Polytri
       ( V2 0 0
       , V2 (r*cos(2*pi*fromFastℕtoℝ m/fromFastℕtoℝ n)) (r*sin(2*pi*fromFastℕtoℝ m/fromFastℕtoℝ n))
       , V2 (r*cos(2*pi*fromFastℕtoℝ (m+1)/fromFastℕtoℝ n)) (r*sin(2*pi*fromFastℕtoℝ (m+1)/fromFastℕtoℝ n))
       )
       | m <- [0.. n-1] ]
    where
      n :: Fastℕ
      n = max 5 $ ceiling $ 2*pi*r/res
symbolicGetContourMesh res obj = getContourMesh (pure res) obj
