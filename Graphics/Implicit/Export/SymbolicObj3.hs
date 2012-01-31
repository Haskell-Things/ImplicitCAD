-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- We just want to export the instance...
module Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh) where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingCubes

import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import Graphics.Implicit.Export.SymbolicObj2

import qualified Graphics.Implicit.SaneOperators as S

import Graphics.Implicit.Export.Symbolic.CoerceSymbolic2
import Graphics.Implicit.Export.Symbolic.CoerceSymbolic3
import Graphics.Implicit.Export.Symbolic.Rebound2
import Graphics.Implicit.Export.Symbolic.Rebound3


instance DiscreteAproxable SymbolicObj3 TriangleMesh where
	discreteAprox res obj = symbolicGetMesh res obj

symbolicGetMesh :: ℝ -> SymbolicObj3 -> [(ℝ3, ℝ3, ℝ3)]

symbolicGetMesh res (Translate3 v obj) = map (\(a,b,c) -> (a S.+ v, b S.+ v, c S.+ v) ) $ symbolicGetMesh res obj
symbolicGetMesh res (Scale3 s obj) =
	let
		mesh :: [(ℝ3, ℝ3, ℝ3)]
		mesh = symbolicGetMesh res obj
		scaleTriangle :: (ℝ3, ℝ3, ℝ3) -> (ℝ3, ℝ3, ℝ3)
		scaleTriangle (a,b,c) = (s S.* a, s S.* b, s S.* c)
	in map scaleTriangle  mesh

symbolicGetMesh _ (Rect3 (x1,y1,z1) (x2,y2,z2)) = 
	let
		square a b c d = [(a,b,c),(d,a,c)]
	in
		   square (x1,y1,z1) (x2,y1,z1) (x2,y2,z1) (x1,y2,z1)
		++ square (x1,y1,z2) (x2,y1,z2) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x2,y1,z1) (x2,y1,z2) (x1,y1,z2)
		++ square (x1,y2,z1) (x2,y2,z1) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x1,y1,z2) (x1,y2,z2) (x1,y2,z1)
		++ square (x2,y1,z1) (x2,y1,z2) (x2,y2,z2) (x2,y2,z1)

symbolicGetMesh res (Sphere r) = 
	let
		square a b c d = [(a,b,c),(d,a,c)]
		n = max 5 (fromIntegral $ ceiling $ 3*r/res)
	in
		concat [ square
		 (r*cos(2*pi*m1/n),     r*sin(2*pi*m1/n)*cos(pi*m2/n),     r*sin(2*pi*m1/n)*sin(pi*m2/n) ) 
		 (r*cos(2*pi*(m1+1)/n), r*sin(2*pi*(m1+1)/n)*cos(pi*m2/n), r*sin(2*pi*(m1+1)/n)*sin(pi*m2/n) ) 
		 (r*cos(2*pi*(m1+1)/n), r*sin(2*pi*(m1+1)/n)*cos(pi*(m2+1)/n), r*sin(2*pi*(m1+1)/n)*sin(pi*(m2+1)/n) ) 
		 (r*cos(2*pi*m1/n),     r*sin(2*pi*m1/n)*cos(pi*(m2+1)/n), r*sin(2*pi*m1/n)*sin(pi*(m2+1)/n)) 
		  | m1 <- [0.. n-1], m2 <- [0.. n-1] ]

symbolicGetMesh res  (ExtrudeR 0.0 obj2 h) = 
	let
		segify (a:b:xs) = (a,b):(segify $ b:xs)
		segify _ = []
		segToSide (x1,y1) (x2,y2) =
			[((x1,y1,0), (x2,y2,0), (x2,y2,h)), ((x1,y1,0), (x2,y2,h), (x1,y1,h)) ]
		segs = concat $ map segify $ symbolicGetContour res obj2
		side_tris = concat $ map (\(a,b) -> segToSide a b) segs
		fill_tris = symbolicGetContourMesh res obj2
		bottom_tris = [((a1,a2,0),(b1,b2,0),(c1,c2,0)) | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
		top_tris = [((a1,a2,h),(b1,b2,h),(c1,c2,h)) | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
	in side_tris ++ bottom_tris ++ top_tris

symbolicGetMesh res  (ExtrudeR r obj2 h) = 
	let
		d = fromIntegral $ ceiling $ r

		(ojb2, ((x1,y1),(x2,y2))) = rebound2 $ coerceSymbolic2 obj2
		newbound = ((x1,y1,0),(x2,y2,d))
		(bottom_obj,(bound1,bound2@ (_,_,d2))) = 
			rebound3 (fst $ coerceSymbolic3 (ExtrudeR r obj2 h), newbound)
		bottom_tris = getMesh bound1 bound2 res bottom_obj
		flip (x,y,z) = (x,y,h-z)
		top_tris = map (\(a,b,c) -> (flip a, flip b, flip c)) bottom_tris

		segify (a:b:xs) = (a,b):(segify $ b:xs)
		segify _ = []
		segToSide (x1,y1) (x2,y2) =
			[((x1,y1,d2), (x2,y2,d2), (x2,y2,h-d2)), ((x1,y1,d2), (x2,y2,h-d2), (x1,y1,h-d2)) ]

		segs = concat $ map segify $ symbolicGetContour res obj2
		side_tris = concat $ map (\(a,b) -> segToSide a b) segs
	in side_tris ++ bottom_tris ++ top_tris 

symbolicGetMesh res  obj = case rebound3 (coerceSymbolic3 obj) of
	(obj, (a,b)) -> getMesh a b res obj 
