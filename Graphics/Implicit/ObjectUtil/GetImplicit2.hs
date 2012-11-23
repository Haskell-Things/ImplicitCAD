-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil
import Data.VectorSpace       
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.List (nub)

getImplicit2 :: SymbolicObj2 -> Obj2

-- Primitives
getImplicit2 (RectR r (P (x1,y1)) (P (x2,y2))) = \(P (x,y)) -> MathUtil.rmaximum r
	[abs (x-dx/2-x1) - dx/2, abs (y-dy/2-y1) - dy/2]
		where (dx, dy) = (x2-x1, y2-y1)

getImplicit2 (Circle r ) = 
	\(P (x,y)) -> sqrt (x**2 + y**2) - r

getImplicit2 (PolygonR r points) = 
	\p -> let
		pair :: Int -> (𝔼2,𝔼2)
		pair n = (points !! n, points !! (mod (n + 1) (length points) ) )
		pairs =  [ pair n | n <- [0 .. (length points) - 1] ]
		relativePairs =  map (\(a,b) -> (a .-. p, b .-. p) ) pairs
		crossing_points =
			[x2 ^-^ y2*(x2-x1)/(y2-y1) | ((x1,y1), (x2,y2)) <- relativePairs,
			   ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
		seemsInRight = odd $ length $ filter (>0) $ nub crossing_points
		seemsInLeft = odd $ length $ filter (<0) $ nub crossing_points
		isIn = seemsInRight && seemsInLeft
		dists = map (MathUtil.distFromLineSeg p) pairs :: [ℝ]
	in
		minimum dists * if isIn then -1 else 1

-- (Rounded) CSG
getImplicit2 (Complement2 symbObj) = 
	let
		obj = getImplicit2 symbObj
	in
		\p -> - obj p

getImplicit2 (UnionR2 r symbObjs) =
	let 
		objs = map getImplicit2 symbObjs
	in
		if r == 0
		then \p -> minimum $ map ($p) objs 
		else \p -> MathUtil.rminimum r $ map ($p) objs

getImplicit2 (DifferenceR2 r symbObjs) =
	let 
		obj:objs = map getImplicit2 symbObjs
		complement obj = \p -> - obj p
	in
		if r == 0
		then \p -> maximum $ map ($p) $ obj:(map complement objs) 
		else \p -> MathUtil.rmaximum r $ map ($p) $ obj:(map complement objs) 

getImplicit2 (IntersectR2 r symbObjs) = 
	let 
		objs = map getImplicit2 symbObjs
	in
		if r == 0
		then \p -> maximum $ map ($p) objs 
		else \p -> MathUtil.rmaximum r $ map ($p) objs

-- Simple transforms
getImplicit2 (Translate2 v symbObj) =
	let
		obj = getImplicit2 symbObj
	in
		\p -> obj (p .-^ v)

getImplicit2 (Scale2 (sx,sy) symbObj) =
	let
		obj = getImplicit2 symbObj
	in
		\(P (x,y)) -> (max sx sy) * obj (P (sx*x, sy*y))

getImplicit2 (Rotate2 θ symbObj) = 
	let
		obj = getImplicit2 symbObj
	in
		\(P (x,y)) -> obj $ P (cos(θ)*x + sin(θ)*y, cos(θ)*y - sin(θ)*x)

-- Boundary mods
getImplicit2 (Shell2 w symbObj) = 
	let
		obj = getImplicit2 symbObj
	in
		\p -> abs (obj p) - w/2

getImplicit2 (Outset2 d symbObj) =
	let
		obj = getImplicit2 symbObj
	in
		\p -> obj p - d

-- Misc
getImplicit2 (EmbedBoxedObj2 (obj,box)) = obj

