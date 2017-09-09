-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is all of this needed?
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- This file symbolicaly renders contours and contour fillings.
-- If it can't, it passes the puck to a marching-squares-like
-- algorithm...

module Graphics.Implicit.Export.SymbolicObj2 where

import Prelude(map, ($), (-), (/), (+), (>), (*), (.), reverse, cos, pi, sin, max, fromInteger, ceiling)

import Graphics.Implicit.Definitions (ℝ, ℝ2, SymbolicObj2(RectR, Circle, Translate2, Scale2), Polyline, (⋯*))

import Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh)

import Graphics.Implicit.ObjectUtil (getImplicit2, getBox2)

import Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2)

import qualified Graphics.Implicit.Export.Render as Render (getContour)

import Data.VectorSpace ((^/), magnitude)

symbolicGetOrientedContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetOrientedContour res symbObj = map orient $ symbolicGetContour res symbObj
    where
        obj = getImplicit2 symbObj
        orient :: Polyline -> Polyline
        orient [] = []
        orient [_] = []
        orient points@(x:y:_) =
            let
                v = (\(a,b) -> (b, -a)) (y - x)
                dv = v ^/ (magnitude v / res / 0.1)
            in if obj (x + dv) - obj x > 0
            then points
            else reverse points

symbolicGetContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetContour _ (RectR 0 (x1,y1) (x2,y2)) = [[ (x1,y1), (x2,y1), (x2,y2), (x1,y2), (x1,y1) ]]
symbolicGetContour res (Circle r) = [[ ( r*cos(2*pi*m/n), r*sin(2*pi*m/n) ) | m <- [0.. n] ]] where
    n :: ℝ
    n = max 5 (fromInteger . ceiling $ 2*pi*r/res)
symbolicGetContour res (Translate2 v obj) = map (map (+ v) ) $ symbolicGetContour res obj
symbolicGetContour res (Scale2 s@(a,b) obj) = map (map (⋯* s)) $ symbolicGetContour (res/sc) obj
    where sc = max a b
symbolicGetContour res obj = case rebound2 (getImplicit2 obj, getBox2 obj) of
    (obj', (a,b)) -> Render.getContour a b res obj'


symbolicGetContourMesh :: ℝ ->  SymbolicObj2 -> [(ℝ2,ℝ2,ℝ2)]
symbolicGetContourMesh res (Translate2 v obj) = map (\(a,b,c) -> (a + v, b + v, c + v) )  $
    symbolicGetContourMesh res obj
symbolicGetContourMesh res (Scale2 s@(a,b) obj) = map (\(c,d,e) -> (c ⋯* s, d ⋯* s, e ⋯* s) )  $
    symbolicGetContourMesh (res/sc) obj where sc = max a b
symbolicGetContourMesh _ (RectR 0 (x1,y1) (x2,y2)) = [((x1,y1), (x2,y1), (x2,y2)), ((x2,y2), (x1,y2), (x1,y1)) ]
symbolicGetContourMesh res (Circle r) =
    [ ((0,0),
       (r*cos(2*pi*m/n), r*sin(2*pi*m/n)),
       (r*cos(2*pi*(m+1)/n), r*sin(2*pi*(m+1)/n))
      )| m <- [0.. n-1] ]
    where
      n :: ℝ
      n = max 5 (fromInteger . ceiling $ 2*pi*r/res)
symbolicGetContourMesh res obj = case rebound2 (getImplicit2 obj, getBox2 obj) of
    (obj', (a,b)) -> getContourMesh a b (res,res) obj'


