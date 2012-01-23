-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.SymbolicObj3 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives.Definitions
import Graphics.Implicit.Primitives.BoxedObj3


instance PrimitiveSupporter3 SymbolicObj3 where
	sphere r = Sphere r
	cubeV (dx, dy, dz) = Rect3 (0,0,0) (dx, dy, dz)
	cylinder2 r1 r2 h = EmbedBoxedObj3 $ cylinder2 r1 r2 h
	torus r_main r_second = EmbedBoxedObj3 $ torus r_main r_second


