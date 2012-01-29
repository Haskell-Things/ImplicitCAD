-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}


module Graphics.Implicit.Operations.BoxedObjPair where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Operations.ObjPair
import Graphics.Implicit.Operations.BoxPair


-- | Operations that are specific to some part of a
--   2D-3D dimensional object pair.
instance PairObj BoxedObj2 ℝ2 BoxedObj3 ℝ3 where

	extrudeR r (obj, box) h = (extrudeR r obj h, extrudeR r box h)
	extrudeRMod r mod (obj, box) h = (extrudeRMod r mod obj h, extrudeRMod r mod box h)
	extrudeOnEdgeOf (obj1, box1) (obj2, box2) = (extrudeOnEdgeOf obj1 obj2, extrudeOnEdgeOf box1 box2)
	rotate3 rot (obj, box) =  (rotate3 rot obj, rotate3 rot box)
