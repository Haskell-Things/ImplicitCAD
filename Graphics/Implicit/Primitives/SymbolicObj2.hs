-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Primitives.SymbolicObj2 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives.Definitions


instance PrimitiveSupporter2 SymbolicObj2 where
	circle   = Circle
	rectR    = RectR
	polygonR = PolygonR

