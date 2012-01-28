-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Export.SymbolicObj2 where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingSquares
import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import qualified Graphics.Implicit.SaneOperators as S

instance DiscreteAproxable SymbolicObj2 [Polyline] where
	discreteAprox res obj = symbolicGetContour res obj


coerceSymbolic2 :: SymbolicObj2 -> BoxedObj2
coerceSymbolic2 (EmbedBoxedObj2 boxedObj) = boxedObj
coerceSymbolic2 (Rect (x1,y1) (x2,y2)) = translate (x1,y1) $ (squareV (x2-x1, y2-y1))
coerceSymbolic2 (Circle r ) = circle r
coerceSymbolic2 (Polygon points) = polygon points
coerceSymbolic2 (UnionR2 r objs) = unionR r (map coerceSymbolic2 objs)
coerceSymbolic2 (IntersectR2 r objs) = intersectR r (map coerceSymbolic2 objs)
coerceSymbolic2 (DifferenceR2 r objs) = differenceR r (map coerceSymbolic2 objs)
coerceSymbolic2 (Complement2 obj) = complement $ coerceSymbolic2 obj
coerceSymbolic2 (Shell2 w obj) = shell w $ coerceSymbolic2 obj
coerceSymbolic2 (Translate2 v obj) = translate v $ coerceSymbolic2 obj
coerceSymbolic2 (Scale2 s obj) = scale s $ coerceSymbolic2 obj
coerceSymbolic2 (Rotate2 a obj) = rotateXY a $ coerceSymbolic2 obj
coerceSymbolic2 (Outset2 d obj) = outset 2 $ coerceSymbolic2 obj


symbolicGetContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetContour _ (Rect (x1,y1) (x2,y2)) = [[ (x1,y1), (x2,y1), (x2,y2), (x1,y2), (x1,y1) ]]
symbolicGetContour res (Circle r) = [[ ( r*cos(2*pi*m/n), r*sin(2*pi*m/n) ) | m <- [0.. n-1] ]] where
	n = max 5 (fromIntegral $ ceiling $ 2*pi*r/res)
symbolicGetContour res (Translate2 v obj) = map (map (S.+ v) ) $ symbolicGetContour res obj
symbolicGetContour res (Scale2 s obj) = map (map (S.* s)) $ symbolicGetContour res obj
symbolicGetContour res obj = (\(obj,(a,b)) ->  
	let
		d :: ℝ2
		d = (b S.- a) S./ (10.0 :: ℝ)
	in 
		getContour (a S.- d) (b S.+ d) (res,res) obj
	) (coerceSymbolic2 obj)


