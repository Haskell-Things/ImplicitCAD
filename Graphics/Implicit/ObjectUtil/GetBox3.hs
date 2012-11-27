-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox3 (getBox3) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import  Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getDist2)

import Debug.Trace

isEmpty :: Box3 -> Bool
isEmpty = (== (origin, origin))

outsetBox :: ℝ -> Box3 -> Box3
outsetBox r (a,b) =
	(a .-^ (r,r,r), b .+^ (r,r,r))

scaleP :: ℝ3 -> 𝔼3 -> 𝔼3
scaleP (sx,sy,sz) (P (x,y,z)) =
	P (sx*x, sy*y, sz*z)

getBox3 :: SymbolicObj3 -> Box3

-- Primitives
getBox3 (Rect3R r a b) = (a,b)

getBox3 (Sphere r ) = (P (-r, -r, -r), P (r,r,r))

getBox3 (Cylinder h r1 r2) = (P (-r,-r,0), P (r,r,h)) where r = max r1 r2

-- (Rounded) CSG
getBox3 (Complement3 symbObj) =
	(P (-infty, -infty, -infty), P (infty, infty, infty)) where infty = 1/0

getBox3 (UnionR3 r symbObjs) = (P (left-r,bot-r,inward-r), P (right+r,top+r,out+r))
	where
		boxes = map getBox3 symbObjs
		isEmpty = (== (origin,origin))
		(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
		(lefts, bots, ins) = unzip3 $ map unPoint leftbot
		(rights, tops, outs) = unzip3 $ map unPoint topright
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
		(lefts, bots, ins) = unzip3 $ map unPoint leftbot
		(rights, tops, outs) = unzip3 $ map unPoint topright
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
		then (P (left,bot,inward), P (right,top,out))
		else (origin,origin)

getBox3 (DifferenceR3 r symbObjs) = firstBox
	where
		firstBox:_ = map getBox3 symbObjs

-- Simple transforms
getBox3 (Translate3 v symbObj) =
	let
		(a,b) = getBox3 symbObj
	in
		(a.+^v, b.+^v)

getBox3 (Scale3 s symbObj) =
	let
		(a,b) = getBox3 symbObj
	in
		(scaleP s a, scaleP s b)

getBox3 (Rotate3 _ symbObj) = ( P (-d, -d, -d), P (d, d, d) )
	where
		(P (x1,y1,z1), P (x2,y2,z2)) = getBox3 symbObj
		d = (sqrt 2 *) $ maximum $ map abs [x1, x2, y1, y2, z1, z2]

-- Boundary mods
getBox3 (Shell3 w symbObj) =
	outsetBox (w/2) $ getBox3 symbObj

getBox3 (Outset3 d symbObj) =
	outsetBox d $ getBox3 symbObj

-- Misc
getBox3 (EmbedBoxedObj3 (obj,box)) = box

-- 2D Based
getBox3 (ExtrudeR r symbObj h) = (P (x1,y1,0), P (x2,y2,h))
	where
		(P (x1,y1), P (x2,y2)) = getBox2 symbObj

getBox3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
	let
		(P (ax1,ay1), P (ax2,ay2)) = getBox2 symbObj1
		(P (bx1,by1), P (bx2,by2)) = getBox2 symbObj2
	in
		(P (bx1+ax1, by1+ax1, ay2), P (bx2+ax2, by2+ax2, ay2))


getBox3 (ExtrudeRM r twist scale translate symbObj eitherh) =
	let
		range = [0, 0.1 .. 1.0]

		(P (x1,y1), P (x2,y2)) = getBox2 symbObj
		(dx,dy) = (x2 - x1, y2 - y1)
		(xrange, yrange) = (map (\s -> x1+s*dx) $ range, map (\s -> y1+s*dy) $ range )

		h = case eitherh of
			Left h -> h
			Right hf -> hmax + 0.2*(hmax-hmin)
				where
					hs = [hf (x,y) | x <- xrange, y <- yrange]
					(hmin, hmax) = (minimum hs, maximum hs)
		
		hrange = map (h*) $ range

		sval = case scale of
			Nothing -> 1
			Just scale' -> maximum $ map (abs . scale') hrange
		
		(twistXmin, twistYmin, twistXmax, twistYmax) = case twist of
			Nothing -> (smin x1, smin y1, smax x2, smax y2)
				where
					smin y = min y (sval * y)
					smax y = max y (sval * y)
			Just _  -> (-d, -d, d, d)
				where d = sval * getDist2 origin symbObj
		
		translate' = fromMaybe (const (0,0)) translate
		(tvalsx, tvalsy) = unzip . map (translate' . (h*)) $ hrange
		(tminx, tminy) = (minimum tvalsx, minimum tvalsy)
		(tmaxx, tmaxy) = (maximum tvalsx, maximum tvalsy)
	in
		(P (-dx+tminx, -dy+tminy, 0), P (dx+tmaxx, dy+tmaxy, h))


getBox3 (RotateExtrude _ _ (Left (xshift,yshift)) symbObj) = 
	let
		(P (x1,y1), P (x2,y2)) = getBox2 symbObj
		r = max x2 (x2 + xshift)
	in
		(P (-r, -r, min y1 (y1 + yshift)), P (r, r, max y2 (y2 + yshift)))

getBox3 (RotateExtrude rot _ (Right f) symbObj) = 
	let
		(P (x1,y1), P (x2,y2)) = getBox2 symbObj
		(xshifts, yshifts) = unzip [f θ | θ <- [0 , rot / 10 .. rot] ]
		xmax = maximum xshifts
		ymax = maximum yshifts
		ymin = minimum yshifts
		xmax' = if xmax > 0 then xmax * 1.1 else if xmax < - x1 then 0 else xmax
		ymax' = ymax + 0.1 * (ymax - ymin)
		ymin' = ymin - 0.1 * (ymax - ymin)
		r = x2 + xmax'
	in
		(P (-r, -r, y1 + ymin'), P (r, r, y2 + ymax'))



