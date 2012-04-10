-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox3 (getBox3) where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.SaneOperators
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil

import  Graphics.Implicit.ObjectUtil.GetBox2 (getBox2)

getBox3 :: SymbolicObj3 -> Box3

-- Primitives
getBox3 (Rect3R r a b) = (a,b)

getBox3 (Sphere r ) = ((-r, -r, -r), (r,r,r))

getBox3 (Cylinder h r1 r2) = ( (-r,-r,0), (r,r,h) ) where r = max r1 r2

-- (Rounded) CSG
getBox3 (Complement3 symbObj) = 
	((-infty, -infty, -infty), (infty, infty, infty)) where infty = (1::ℝ)/(0 ::ℝ)

getBox3 (UnionR3 r symbObjs) = ((left,bot,inward), (right,top,out))
	where 
		boxes = map getBox3 symbObjs
		isEmpty = ( == ((0,0,0),(0,0,0)) )
		(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
		(lefts, bots, ins) = unzip3 leftbot
		(rights, tops, outs) = unzip3 topright
		left = minimum lefts
		bot = minimum bots
		inward = minimum ins
		right = maximum rights
		top = maximum tops
		out = maximum outs

getBox3 (IntersectR3 r symbObjs) = 
	let 
		boxes = map getBox3 symbObjs
		(leftbot, topright) = unzip boxes
		(lefts, bots, ins) = unzip3 leftbot
		(rights, tops, outs) = unzip3 topright
		left = maximum lefts
		bot = maximum bots
		inward = maximum ins
		right = minimum rights
		top = minimum tops
		out = minimum outs
	in
		if   top   > bot 
		  && right > left 
		  && out   > inward
		then ((left,bot,inward),(right,top,out))
		else ((0,0,0),(0,0,0))

getBox3 (DifferenceR3 r symbObjs) = firstBox
	where
		firstBox:_ = map getBox3 symbObjs

-- Simple transforms
getBox3 (Translate3 v symbObj) =
	let
		(a,b) = getBox3 symbObj
	in
		(a+v, b+v)

getBox3 (Scale3 s symbObj) =
	let
		(a,b) = getBox3 symbObj
	in
		(s ⋯* a, s ⋯* b)

getBox3 (Rotate3 _ symbObj) = ( (-d, -d, -d), (d, d, d) )
	where
		((x1,y1, z1), (x2,y2, z2)) = getBox3 symbObj
		d = (sqrt (2::ℝ) *) $ maximum $ map abs [x1, x2, y1, y2, z1, z2]

-- Boundary mods
getBox3 (Shell3 w symbObj) = 
	let
		(a,b) = getBox3 symbObj
		d = w/(2.0::ℝ)
	in
		(a - (d,d,d), b + (d,d,d))

getBox3 (Outset3 d symbObj) =
	let
		(a,b) = getBox3 symbObj
	in
		(a - (d,d,d), b + (d,d,d))

-- Misc
getBox3 (EmbedBoxedObj3 (obj,box)) = box

-- 2D Based
getBox3 (ExtrudeR r symbObj h) = ((x1,y1,0),(x2,y2,h))
	where
		((x1,y1),(x2,y2)) = getBox2 symbObj

getBox3 (ExtrudeRMod r mod symbObj h) = 
	let
		((x1,y1),(x2,y2)) = getBox2 symbObj
		dx = x2 - x1
		dy = y2 - y1
	in
		((x1 - dx, y1 - dy, 0),(x2 + dx, y2+ dy, h)) 

getBox3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
	let
		((ax1,ay1),(ax2,ay2)) = getBox2 symbObj1
		((bx1,by1),(bx2,by2)) = getBox2 symbObj2
	in
		((bx1+ax1, by1+ax1, ay2), (bx2+ax2, by2+ax2, ay2))

