-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.SymbolicManip where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

coerceSymbolic2 :: SymbolicObj2 -> BoxedObj2
coerceSymbolic2 (EmbedBoxedObj2 boxedObj) = boxedObj
--coerceSymbolic2 (Rect (x1,y1) (x2,y2)) = translate (x1,y1) $ (cubeV (x2-x1, y2-y1) :: Boxed2 Obj2)
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
coerceSymbolic2 (EmbedBoxedObj3 boxedObj) = boxedObj
--coerceSymbolic2 (Rect (x1,y1) (x2,y2)) = translate (x1,y1) $ (cubeV (x2-x1, y2-y1) :: Boxed2 Obj2)
coerceSymbolic2 (Sphere r ) = sphere r
coerceSymbolic2 (UnionR3 r objs) = unionR r (map coerceSymbolic3 objs)
coerceSymbolic2 (IntersectR3 r objs) = intersectR r (map coerceSymbolic3 objs)
coerceSymbolic2 (DifferenceR3 r objs) = differenceR r (map coerceSymbolic3 objs)
coerceSymbolic2 (Complement3 obj) = complement $ coerceSymbolic3 obj
coerceSymbolic2 (Shell3 w obj) = shell w $ coerceSymbolic3 obj
coerceSymbolic2 (Translate3 v obj) = translate v $ coerceSymbolic3 obj
coerceSymbolic2 (Scale3 s obj) = scale s $ coerceSymbolic3 obj
coerceSymbolic2 (Outset3 d obj) = outset 2 $ coerceSymbolic3 obj
