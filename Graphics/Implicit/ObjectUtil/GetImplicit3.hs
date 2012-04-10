-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetImplicit3 (getImplicit3) where

import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.SaneOperators
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil

import  Graphics.Implicit.ObjectUtil.GetImplicit2 (getImplicit2)

getImplicit3 :: SymbolicObj3 -> Obj3

-- Primitives
getImplicit3 (Rect3R r (x1,y1,z1) (x2,y2,z2)) = \(x,y,z) -> MathUtil.rmaximum r
	[abs (x-dx/(2::ℝ)-x1) - dx/(2::ℝ), abs (y-dy/(2::ℝ)-y1) - dy/(2::ℝ), abs (z-dz/(2::ℝ)-z1) - dz/(2::ℝ)]
		where (dx, dy, dz) = (x2-x1, y2-y1, z2-z1)

getImplicit3 (Sphere r ) = 
	\(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r

getImplicit3 (Cylinder h r1 r2) = \(x,y,z) ->
	let
		d = sqrt(x^2+y^2) - ((r2-r1)/h*z+r1)
		θ = atan2 (r2-r1) h
	in
		max (d * cos θ) (abs(z-h/(2::ℝ)) - h/(2::ℝ))

-- (Rounded) CSG
getImplicit3 (Complement3 symbObj) = 
	let
		obj = getImplicit3 symbObj
	in
		\p -> - obj p

getImplicit3 (UnionR3 r symbObjs) =
	let 
		objs = map getImplicit3 symbObjs
	in
		if r == 0
		then \p -> minimum $ map ($p) objs 
		else \p -> MathUtil.rminimum r $ map ($p) objs

getImplicit3 (IntersectR3 r symbObjs) = 
	let 
		objs = map getImplicit3 symbObjs
	in
		if r == 0
		then \p -> maximum $ map ($p) objs 
		else \p -> MathUtil.rmaximum r $ map ($p) objs

getImplicit3 (DifferenceR3 r symbObjs) =
	let 
		obj:objs = map getImplicit3 symbObjs
		complement obj = \p -> - obj p
	in
		if r == 0
		then \p -> maximum $ map ($p) $ obj:(map complement objs) 
		else \p -> MathUtil.rmaximum r $ map ($p) $ obj:(map complement objs) 

-- Simple transforms
getImplicit3 (Translate3 v symbObj) =
	let
		obj = getImplicit3 symbObj
	in
		\p -> obj (p-v)

getImplicit3 (Scale3 s@(sx,sy,sz) symbObj) =
	let
		obj = getImplicit3 symbObj
	in
		\p -> (maximum [sx, sy, sz]) * obj (p ⋯/ s)

getImplicit3 (Rotate3 (yz, xz, xy) symbObj) = 
	let
		obj = getImplicit3 symbObj
		rotateYZ :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
		rotateYZ θ obj = \(x,y,z) -> obj ( x, cos(θ)*z - sin(θ)*y, cos(θ)*y + sin(θ)*z)
		rotateXZ :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
		rotateXZ θ obj = \(x,y,z) -> obj ( cos(θ)*x + sin(θ)*z, y, cos(θ)*z - sin(θ)*x)
		rotateXY :: ℝ -> (ℝ3 -> ℝ) -> (ℝ3 -> ℝ)
		rotateXY θ obj = \(x,y,z) -> obj ( cos(θ)*x + sin(θ)*y, cos(θ)*y - sin(θ)*x, z)
	in
		rotateYZ yz $ rotateXZ xz $ rotateXY xy $ obj

-- Boundary mods
getImplicit3 (Shell3 w symbObj) = 
	let
		obj = getImplicit3 symbObj
	in
		\p -> abs (obj p) - w/(2::ℝ)

getImplicit3 (Outset3 d symbObj) =
	let
		obj = getImplicit3 symbObj
	in
		\p -> obj p - d

-- Misc
getImplicit3 (EmbedBoxedObj3 (obj,box)) = obj

-- 2D Based
getImplicit3 (ExtrudeR r symbObj h) = 
	let
		obj = getImplicit2 symbObj
	in
		\(x,y,z) -> MathUtil.rmax r (obj (x,y)) (abs (z - h/(2::ℝ)) - h/(2::ℝ))

getImplicit3 (ExtrudeRMod r mod symbObj h) = 
	let
		obj = getImplicit2 symbObj
	in
		\(x,y,z) -> MathUtil.rmax r (obj $ mod z (x,y)) (abs (z - h/(2::ℝ)) - h/(2::ℝ))

getImplicit3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
	let
		obj1 = getImplicit2 symbObj1
		obj2 = getImplicit2 symbObj2
	in
		\(x,y,z) -> obj1 (obj2 (x,y), z)
