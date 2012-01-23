-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations.SymbolicObj3 where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators


instance BasicObj SymbolicObj3 ℝ3 where
	translate = Translate3
	scale     = Scale3
	rotateXY  = Rotate3
	complement= Complement3
	union     = UnionR3 0
	intersect = IntersectR3 0
	difference= DifferenceR3 0

instance MagnitudeObj SymbolicObj3 where
	outset      = Outset3
	shell       = Shell3
	unionR      = UnionR3 
	intersectR  = UnionR3 
	differenceR = DifferenceR3
