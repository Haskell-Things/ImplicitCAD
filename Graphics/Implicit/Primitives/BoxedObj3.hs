-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.BoxedObj3 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives.Definitions
import Graphics.Implicit.Primitives.Obj3


instance PrimitiveSupporter3 (Boxed3 Obj3) where
	sphere r = (sphere r, ((-r, -r, -r), (r,r,r)) )
	cubeV (dx, dy, dz) = (cubeV (dx, dy, dz), ( (0,0,0), (dx, dy, dz) ) )
	cylinder2 r1 r2 h = (cylinder2 r1 r2 h, ( (-r,-r,0), (r,r,h) ) ) where r = max r1 r2
	torus r_main r_second = (torus r_main r_second, ((-r, -r, -r_second), (r, r, r_second)))
		where r = r_main + r_second


