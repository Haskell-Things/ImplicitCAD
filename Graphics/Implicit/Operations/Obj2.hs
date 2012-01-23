-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations.Obj2 where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators


instance BasicObj Obj2 ℝ2 where
	translate p obj = \q -> obj (q-p)
	scale s obj = \p -> s * obj (p/s)
	rotateXY θ obj = \(x,y) -> obj ( cos(θ)*x + sin(θ)*y, cos(θ)*y - sin(θ)*x)
	complement obj = \p -> - obj p
	union objs = \p -> minimum $ map ($p) objs
	intersect objs = \p -> maximum $ map ($p) objs
	difference (obj:objs) = \p -> maximum $ map ($p) $ obj:(map complement objs)

instance MagnitudeObj Obj2 where
	outset d obj = \p -> obj p - d
	shell w a = \p -> abs (a p) - w/(2.0::ℝ)
	unionR r objs = \p -> rminimum r $ map ($p) objs
	intersectR r objs = \p -> rmaximum r $ map ($p) objs
	differenceR r (x:xs) = \p -> rmaximum r $ (x p) :(map (negate . ($p)) xs)
