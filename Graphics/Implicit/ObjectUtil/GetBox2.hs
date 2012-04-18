-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2) where

import Prelude hiding ((+),(-),(*),(/))
import qualified Prelude as P
import Graphics.Implicit.SaneOperators
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil
import Data.List (nub)

getBox2 :: SymbolicObj2 -> Box2

-- Primitives
getBox2 (RectR r a b) = (a,b)

getBox2 (Circle r ) =  ((-r, -r), (r,r))

getBox2 (PolygonR r points) = ((minimum xs, minimum ys), (maximum xs, maximum ys)) 
	 where (xs, ys) = unzip points

-- (Rounded) CSG
getBox2 (Complement2 symbObj) = 
	((-infty, -infty), (infty, infty)) where infty = (1::ℝ)/(0 ::ℝ)

getBox2 (UnionR2 r symbObjs) =
	let 
		boxes = map getBox2 symbObjs
		isEmpty = ( == ((0,0),(0,0)) )
		(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
		(lefts, bots) = unzip leftbot
		(rights, tops) = unzip topright
		left = minimum lefts
		bot = minimum bots
		right = maximum rights
		top = maximum tops
	in
		((left-r,bot-r),(right+r,top+r))

getBox2 (DifferenceR2 r symbObjs) =
	let 
		firstBox:_ = map getBox2 symbObjs
	in
		firstBox

getBox2 (IntersectR2 r symbObjs) = 
	let 
		boxes = map getBox2 symbObjs
		(leftbot, topright) = unzip boxes
		(lefts, bots) = unzip leftbot
		(rights, tops) = unzip topright
		left = maximum lefts
		bot = maximum bots
		right = minimum rights
		top = minimum tops
	in
		((left-r,bot-r),(right+r,top+r))

-- Simple transforms
getBox2 (Translate2 v symbObj) =
	let
		(a,b) = getBox2 symbObj
	in
		if (a,b) == ((0,0),(0,0))
		then ((0,0),(0,0))
		else (a+v, b+v)

getBox2 (Scale2 s symbObj) =
	let
		(a,b) = getBox2 symbObj
	in
		(s ⋯* a, s ⋯* b)

getBox2 (Rotate2 θ symbObj) = 
	let
		((x1,y1),(x2,y2)) = getBox2 symbObj
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

-- Boundary mods
getBox2 (Shell2 w symbObj) = 
	let
		(a,b) = getBox2 symbObj
		d = w/(2.0::ℝ)
	in
		(a - (d,d), b + (d,d))

getBox2 (Outset2 d symbObj) =
	let
		(a,b) = getBox2 symbObj
	in
		(a - (d,d), b + (d,d))

-- Misc
getBox2 (EmbedBoxedObj2 (obj,box)) = box
