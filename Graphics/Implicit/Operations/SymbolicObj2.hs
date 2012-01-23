-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations.SymbolicObj2 where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators


instance BasicObj SymbolicObj2 ℝ2 where
	translate = Translate2
	scale     = Scale2
	rotateXY  = Rotate2
	complement= Complement2
	union     = UnionR2 0
	intersect = IntersectR2 0
	difference= DifferenceR2 0

instance MagnitudeObj SymbolicObj2 where
	outset      = Outset2
	shell       = Shell2 
	unionR      = UnionR2 
	intersectR  = UnionR2 
	differenceR = DifferenceR2
