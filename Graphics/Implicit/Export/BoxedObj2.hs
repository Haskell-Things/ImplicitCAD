-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Export.BoxedObj2 where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingSquares

instance DiscreteAproxable BoxedObj2 [Polyline] where
	discreteAprox res (obj,(a,b)) = getContour a b (res,res) obj
