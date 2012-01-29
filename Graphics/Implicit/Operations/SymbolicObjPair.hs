-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Operations.SymbolicObjPair where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations.Definitions


-- | Operations that are specific to some part of a
--   2D-3D dimensional object pair.
instance PairObj SymbolicObj2 ℝ2 SymbolicObj3 ℝ3 where

	extrudeR = ExtrudeR 
	extrudeRMod = ExtrudeRMod
	extrudeOnEdgeOf = ExtrudeOnEdgeOf
	rotate3 = Rotate3

