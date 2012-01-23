-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations.BoxedObj2 where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Operations.Box2
import Graphics.Implicit.Operations.Obj2
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators


instance BasicObj (Boxed2 Obj2) ℝ2 where
	translate p (obj, box) = (translate p obj, translate p box)
	scale s (obj, box) = (scale s obj, scale s box)
	rotateXY θ (obj, box) = (rotateXY θ obj, rotateXY θ box)
	complement (obj, box) = (complement obj, complement box )
	union bobjs = (union objs, union boxes) where
		(objs, boxes) = unzip bobjs
	intersect bobjs = (intersect objs, intersect boxes) where
		(objs, boxes) = unzip bobjs
	difference bobjs = (difference objs, difference boxes) where
		(objs, boxes) = unzip bobjs


instance MagnitudeObj (Boxed2 Obj2) where
	outset  d     (obj, box) = (outset d obj, outset d box)
	shell   w     (obj, box) = (shell  w obj, shell  w box)
	unionR      r bobjs      = (unionR      r objs, unionR      r boxes) where
		(objs, boxes) = unzip bobjs
	intersectR  r bobjs      = (intersectR  r objs, intersectR  r boxes) where
		(objs, boxes) = unzip bobjs
	differenceR r bobjs      = (differenceR r objs, differenceR r boxes) where
		(objs, boxes) = unzip bobjs
