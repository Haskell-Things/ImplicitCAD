-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.SymbolicManip where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives
import Graphics.Implicit.Tracing

import qualified Graphics.Implicit.SaneOperators as S

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


symbolicGetContour :: ℝ ->  SymbolicObj2 -> [ℝ2]
symbolicGetContour _ (Rect (x1,y1) (x2,y2)) = [ (x1,y1), (x2,y1), (x2,y2), (x1,y2), (x1,y1) ]
symbolicGetContour res (Circle r) = [ ( r*cos(2*pi*m/n), r*sin(2*pi*m/n) ) | m <- [0.. n-1] ] where
	n = max 5 (fromIntegral $ ceiling $ 2*pi*r/res)
symbolicGetContour res (Translate2 v obj) = map (S.+ v) $ symbolicGetContour res obj
symbolicGetContour res (Scale2 s obj) = map (S.* s) $ symbolicGetContour res obj
symbolicGetContour res obj = (\(obj,(a,b)) -> getContour a b res obj) (coerceSymbolic2 obj)


symboicGetMesh :: ℝ -> SymbolicObj3 -> [(ℝ3, ℝ3, ℝ3)]
symboicGetMesh _ (Rect3 (x1,y1,z1) (x2,y2,z2)) = 
	let
		square a b c d = [(a,b,c),(d,a,c)]
	in
		   square (x1,y1,z1) (x2,y1,z1) (x2,y2,z1) (x1,y2,z1)
		++ square (x1,y1,z2) (x2,y1,z2) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x2,y1,z1) (x2,y1,z2) (x1,y1,z2)
		++ square (x1,y2,z1) (x2,y2,z1) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x1,y1,z2) (x1,y2,z2) (x1,y2,z1)
		++ square (x2,y1,z1) (x2,y1,z2) (x2,y2,z2) (x2,y2,z1)
symboicGetMesh res  obj = (\(obj,(a,b)) -> getMesh a b res obj) (coerceSymbolic3 obj)

