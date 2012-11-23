-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getDist2) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil
import Data.List (nub)
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

isEmpty :: Box2 -> Bool
isEmpty = (== (origin,origin))

pointsBox :: [𝔼2] -> Box2
pointsBox points =
	let
		(xs, ys) = unzip $ map unPoint points
	in
		(P (minimum xs, minimum ys), P (maximum xs, maximum ys))

unionBoxes :: [Box2] -> Box2
unionBoxes boxes =
	let
		(leftbot, topright) = unzip $ filter (not.isEmpty) boxes
		(lefts, bots) = unzip $ map unPoint leftbot
		(rights, tops) = unzip $ map unPoint topright
	in
		(P (minimum lefts, minimum bots), P (maximum rights, maximum tops))

outsetBox :: ℝ -> Box2 -> Box2
outsetBox r (a,b) =
		(a .-^ (r,r), b .+^ (r,r))

getBox2 :: SymbolicObj2 -> Box2

-- Primitives
getBox2 (RectR r a b) = (a,b)

getBox2 (Circle r ) =  (origin .-^ (r,r), origin .+^ (r,r))

getBox2 (PolygonR r points) = (P (minimum xs, minimum ys), P (maximum xs, maximum ys)) 
	 where (xs, ys) = unzip $ map unPoint points

-- (Rounded) CSG
getBox2 (Complement2 symbObj) = 
	(P (-infty, -infty), P (infty, infty)) where infty = 1/0

getBox2 (UnionR2 r symbObjs) =
	outsetBox r $ unionBoxes (map getBox2 symbObjs)

getBox2 (DifferenceR2 r symbObjs) =
	let 
		firstBox:_ = map getBox2 symbObjs
	in
		firstBox

getBox2 (IntersectR2 r symbObjs) = 
	outsetBox r $ unionBoxes (map getBox2 symbObjs)

-- Simple transforms
getBox2 (Translate2 v symbObj) =
	let
		(a,b) = getBox2 symbObj
	in
		if isEmpty (a,b)
		then (origin,origin)
		else (a.+^v, b.+^v)

getBox2 (Scale2 (sx,sy) symbObj) =
	let
		(P (ax,ay), P (bx,by)) = getBox2 symbObj
	in
		(P (sx*ax, sy*ay), P (sx*bx, sy*by))

getBox2 (Rotate2 θ symbObj) = 
	let
		(P (x1,y1),P (x2,y2)) = getBox2 symbObj
		rotate (x,y) = P (cos(θ)*x - sin(θ)*y, sin(θ)*x + cos(θ)*y)
	in
		pointsBox [ rotate (x1, y1)
				  , rotate (x1, y2)
				  , rotate (x2, y1)
				  , rotate (x2, y2)
				  ]

-- Boundary mods
getBox2 (Shell2 w symbObj) = 
	outsetBox (w/2) $ getBox2 symbObj

getBox2 (Outset2 d symbObj) =
	outsetBox d $ getBox2 symbObj

-- Misc
getBox2 (EmbedBoxedObj2 (obj,box)) = box

-- Get the maximum distance (read upper bound) an object is from a point.
-- Sort of a circular 

getDist2 :: 𝔼2 -> SymbolicObj2 -> ℝ

getDist2 p (UnionR2 r objs) = r + maximum [getDist2 p obj | obj <- objs ]

getDist2 p (Translate2 v obj) = getDist2 (p .+^ v) obj

getDist2 p (Circle r) = distance p origin + r

getDist2 (P (x,y)) symbObj =
	let
		(P (x1,y1), P (x2,y2)) = getBox2 symbObj
	in
		sqrt ((max (abs (x1 - x)) (abs (x2 - x)))^2 + (max (abs (y1 - y)) (abs (y2 - y)))^2)

getDist2 p (PolygonR r points) = 
	r + maximum [p `distance` p' | p' <- points]
