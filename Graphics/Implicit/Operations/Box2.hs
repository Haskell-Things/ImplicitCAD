
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Operations.Box2 where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Operations.Definitions
import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.SaneOperators


-- CSG on 2D boxes
-- Not precise, since not all CSG of such is a 2D box, 
-- but result will be a super set. We will use this for bounding boxes.
-- Empty boxes will always be ((0,0),(0,0)) for convenience :)
instance BasicObj Box2 ℝ2 where
	translate _ ((0,0),(0,0)) = ((0,0),(0,0))
	translate p (a,b) = (a+p, b+p)
	scale s (a,b) = (s*a, s*b)
	rotateXY θ ((x1,y1),(x2,y2)) = 
		let
			rotate (x,y) = ( cos(θ)*x + sin(θ)*y, cos(θ)*y - sin(θ)*x)
			(xa, ya) = rotate (x1, y1)
			(xb, yb) = rotate (x1, y2)
			(xc, yc) = rotate (x2, y1)
			(xd, yd) = rotate (x2, y2)
			minx = minimum [xa, xb, xc, xd]
			miny = minimum [ya, yb, yc, yd]
			maxx = maximum [xa, xb, xc, xd]
			maxy = maximum [ya, yb, yc, yd]
		in
			((minx, miny), (maxx, maxy))
	complement _ = ((-infty, -infty), (infty, infty))
	union boxes = ((left,bot),(right,top)) where
			isEmpty = ( == ((0,0),(0,0)) )
			(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
			(lefts, bots) = unzip leftbot
			(rights, tops) = unzip topright
			left = minimum lefts
			bot = minimum bots
			right = maximum rights
			top = maximum tops
	intersect boxes = 
		let
			(leftbot, topright) = unzip boxes
			(lefts, bots) = unzip leftbot
			(rights, tops) = unzip topright
			left = maximum lefts
			bot = maximum bots
			right = minimum rights
			top = minimum tops
		in
			if top > bot && right > left 
			then ((left,bot),(right,top))
			else ((0,0),(0,0))
	difference (firstBox : otherBoxes) = firstBox


instance MagnitudeObj Box2 where
	outset d (a,b) = (a - (d,d), b + (d,d))
	shell w (a,b) = (a - (w/(2.0::ℝ),w/(2.0::ℝ)), b + (w/(2.0::ℝ),w/(2.0::ℝ)))
	unionR r boxes = outset r $ union boxes
	intersectR r boxes = outset r $ intersect boxes
	differenceR r boxes = outset r $ difference boxes
