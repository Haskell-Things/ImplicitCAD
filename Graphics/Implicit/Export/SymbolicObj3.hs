-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- We just want to export the instance...
module Graphics.Implicit.Export.SymbolicObj3 () where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingCubes

import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import qualified Graphics.Implicit.SaneOperators as S

instance DiscreteAproxable SymbolicObj3 TriangleMesh where
	discreteAprox res obj = symbolicGetMesh res obj

coerceSymbolic3 :: SymbolicObj3 -> BoxedObj3
coerceSymbolic3 (EmbedBoxedObj3 boxedObj) = boxedObj
coerceSymbolic3 (Rect3 (x1,y1,z1) (x2,y2,z2)) = translate (x1,y1,z1) $ (cubeV (x2-x1, y2-y1, z2-z1))
coerceSymbolic3 (Sphere r ) = sphere r
coerceSymbolic3 (UnionR3 r objs) = unionR r (map coerceSymbolic3 objs)
coerceSymbolic3 (IntersectR3 r objs) = intersectR r (map coerceSymbolic3 objs)
coerceSymbolic3 (DifferenceR3 r objs) = differenceR r (map coerceSymbolic3 objs)
coerceSymbolic3 (Complement3 obj) = complement $ coerceSymbolic3 obj
coerceSymbolic3 (Shell3 w obj) = shell w $ coerceSymbolic3 obj
coerceSymbolic3 (Translate3 v obj) = translate v $ coerceSymbolic3 obj
coerceSymbolic3 (Scale3 s obj) = scale s $ coerceSymbolic3 obj
coerceSymbolic3 (Outset3 d obj) = outset 2 $ coerceSymbolic3 obj

symbolicGetMesh :: ℝ -> SymbolicObj3 -> [(ℝ3, ℝ3, ℝ3)]
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
symbolicGetMesh res  obj = (\(obj,(a,b)) -> getMesh a b res obj) (coerceSymbolic3 obj)

