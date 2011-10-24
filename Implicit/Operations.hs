-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE


module Implicit.Operations (
	translate, 
	scale,
	complement,
	union,  intersect,  difference,
	unionR, intersectR, differenceR,
	unionL, intersectL, differenceL,
	shell,
	slice
) where

import Prelude hiding ((+),(-),(*),(/))
import Implicit.Definitions
import Implicit.MathUtil
import Implicit.SaneOperators


translate :: (Additive a a a, AdditiveInvertable a) => a -> (a -> b) -> a -> b
translate p obj = \q -> obj (q-p)

scale s obj = \p -> s * obj (p/s)

complement obj = \p -> - obj p

shell r a = \p -> abs (a p) - r

unionR r a b = \p -> rmin r (a p) (b p)
intersectR r a b = \p -> rmax r (a p) (b p)
differenceR r a b = \p -> rmax r (a p) (- b p)

unionL objs = \p -> minimum $ map ($p) objs
intersectL objs = \p -> maximum $ map ($p) objs
differenceL (obj:objs) = \p -> maximum $ map ($p) $ obj:(map complement objs)




union a b = unionL [a,b]
(∪) a b = union a b
intersect a b = intersectL [a,b]
(∩) a b = intersect a b
difference a b = differenceL [a,b]

slice :: Float -> Obj3 -> Obj2
slice z obj = \(a,b) -> obj (a,b,z)

-- buble :: Obj2 -> Obj3

