-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- The purpose of this function is to symbolicaly compute triangle meshes where possible.
-- Otherwise we coerce it into an implicit function and apply our modified marching cubes algorithm.

-- We just want to export the instance...
module Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh) where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingCubes

import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import Graphics.Implicit.Export.SymbolicObj2

import qualified Graphics.Implicit.SaneOperators as S

import Graphics.Implicit.Export.Symbolic.CoerceSymbolic2
import Graphics.Implicit.Export.Symbolic.CoerceSymbolic3
import Graphics.Implicit.Export.Symbolic.Rebound2
import Graphics.Implicit.Export.Symbolic.Rebound3
import Graphics.Implicit.Export.Util (divideMeshTo, dividePolylineTo)


instance DiscreteAproxable SymbolicObj3 TriangleMesh where
	discreteAprox res obj = symbolicGetMesh res obj

symbolicGetMesh :: ℝ -> SymbolicObj3 -> [(ℝ3, ℝ3, ℝ3)]

-- A translated objects mesh is its mesh translated.
symbolicGetMesh res (Translate3 v obj) = 
	map (\(a,b,c) -> (a S.+ v, b S.+ v, c S.+ v) ) (symbolicGetMesh res obj)

-- A scaled objects mesh is its mesh scaled
symbolicGetMesh res (Scale3 s obj) =
	let
		mesh :: [(ℝ3, ℝ3, ℝ3)]
		mesh = symbolicGetMesh res obj
		scaleTriangle :: (ℝ3, ℝ3, ℝ3) -> (ℝ3, ℝ3, ℝ3)
		scaleTriangle (a,b,c) = (s S.* a, s S.* b, s S.* c)
	in map scaleTriangle  mesh

-- A couple triangles make a cube...
symbolicGetMesh _ (Rect3R 0 (x1,y1,z1) (x2,y2,z2)) = 
	let
		square a b c d = [(a,b,c),(d,a,c)]
	in
		   square (x1,y1,z1) (x2,y1,z1) (x2,y2,z1) (x1,y2,z1)
		++ square (x1,y1,z2) (x2,y1,z2) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x2,y1,z1) (x2,y1,z2) (x1,y1,z2)
		++ square (x1,y2,z1) (x2,y2,z1) (x2,y2,z2) (x1,y2,z2)
		++ square (x1,y1,z1) (x1,y1,z2) (x1,y2,z2) (x1,y2,z1)
		++ square (x2,y1,z1) (x2,y1,z2) (x2,y2,z2) (x2,y2,z1)

-- Use spherical coordiantes to create an easy tesselation of a sphere
symbolicGetMesh res (Sphere r) = 
	let
		square a b c d = [(a,b,c),(d,a,c)]
		n = max 5 (fromIntegral $ ceiling $ 3*r/res)
	in
		concat [ square
		 (r*cos(2*pi*m1/n),     r*sin(2*pi*m1/n)*cos(pi*m2/n),     r*sin(2*pi*m1/n)*sin(pi*m2/n) ) 
		 (r*cos(2*pi*(m1+1)/n), r*sin(2*pi*(m1+1)/n)*cos(pi*m2/n), r*sin(2*pi*(m1+1)/n)*sin(pi*m2/n) ) 
		 (r*cos(2*pi*(m1+1)/n), r*sin(2*pi*(m1+1)/n)*cos(pi*(m2+1)/n), r*sin(2*pi*(m1+1)/n)*sin(pi*(m2+1)/n) ) 
		 (r*cos(2*pi*m1/n),     r*sin(2*pi*m1/n)*cos(pi*(m2+1)/n), r*sin(2*pi*m1/n)*sin(pi*(m2+1)/n)) 
		  | m1 <- [0.. n-1], m2 <- [0.. n-1] ]

-- We can compute a mesh of a rounded, extruded object from it contour, 
-- contour filling trinagles, and magic.
-- General approach:
--   - generate sides by basically cross producting the contour.
--   - generate the the top by taking the contour fill and
--     calculating an appropriate z height.
symbolicGetMesh res  (ExtrudeR r obj2 h) = 
	let
		-- Get a Obj2 (magnitude descriptor object)
		obj2mag :: ℝ2 -> ℝ -- Obj2
		obj2mag = fst $ coerceSymbolic2 obj2
		-- The amount that a point (x,y) on the top should be lifted
		-- from h-r. Because of rounding, the edges should be h-r,
		-- but it should increase inwards.
		dh x y = sqrt (r^2 - ( max 0 $ min r $ r+obj2mag (x,y))^2)
		-- Turn a polyline into a list of its segments
		segify (a:b:xs) = (a,b):(segify $ b:xs)
		segify _ = []
		-- Turn a segment a--b into a list of triangles forming (a--b)×(r,h-r)
		-- The dh stuff is to compensate for rounding errors, etc, and ensure that
		-- the sides meet the top and bottom
		segToSide (x1,y1) (x2,y2) =
			[((x1,y1,r-dh x1 y1), (x2,y2,r-dh x2 y2), (x2,y2,h-r+dh x2 y2)), 
			 ((x1,y1,r-dh x1 y1), (x2,y2,h-r+dh x2 y2), (x1,y1,h-r+dh x1 y1)) ]
		-- Get a contour polyline for obj2, turn it into a list of segments
		segs = concat $ map segify $ symbolicGetContour res obj2
		-- Create sides for the main body of our object = segs × (r,h-r)
		side_tris = concat $ map (\(a,b) -> segToSide a b) segs
		-- Triangles that fill the contour. Make sure the mesh is at least (res/5) fine.
		-- --res/5 because xyres won't always match up with normal res and we need to compensate.
		fill_tris = {-divideMeshTo (res/5) $-} symbolicGetContourMesh res obj2
		-- The bottom. Use dh to determine the z coordinates
		bottom_tris = [((a1,a2,r-dh a1 a2), (b1,b2,r - dh b1 b2), (c1,c2,r - dh c1 c2)) 
				| ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
		-- Same idea at the top.
		top_tris = [((a1,a2,h-r+dh a1 a2), (b1,b2,h-r+dh b1 b2), (c1,c2,h-r+dh c1 c2)) 
				| ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
	in
		-- Merge them all together! :)
		side_tris ++ bottom_tris ++ top_tris 


-- This is quite similar to the one above
-- Key differences are the seperation of the middle part into many layers,
-- and the final transform.
symbolicGetMesh res  (ExtrudeRMod r mod obj2 h) = 
	let
		-- Get a Obj2 (magnitude descriptor object)
		obj2mag :: Obj2 -- = ℝ2 -> ℝ
		obj2mag = fst $ coerceSymbolic2 obj2
		-- The amount that a point (x,y) on the top should be lifted
		-- from h-r. Because of rounding, the edges should be h-r,
		-- but it should increase inwards.
		dh x y = sqrt (r^2 - ( max 0 $ min r $ r+obj2mag (x,y))^2)
		-- Turn a polyline into a list of its segments
		segify (a:b:xs) = (a,b):(segify $ b:xs)
		segify _ = []
		-- The number of steps we're going to do the sides in:
		n = fromIntegral $ ceiling $ h/res
		-- Turn a segment a--b into a list of triangles forming 
		--    (a--b)×(r+(h-2r)*m/n,r+(h-2r)*(m+1)/n)
		-- The dh stuff is to compensate for rounding errors, etc, and ensure that
		-- the sides meet the top and bottom
		-- m is the number of n steps we are up from the base of the main section
		segToSide m (x1,y1) (x2,y2) =
			let
				-- Change across the main body of the object,
				-- at (x1,y1) and (x2,y2) respectivly
				mainH1 = h - 2*r + 2*dh x1 y1
				mainH2 = h - 2*r + 2*dh x2 y2
				-- level a (lower) and level b (upper)
				la1 = r-dh x1 y1  +  mainH1*m/n
				lb1 = r-dh x1 y1  +  mainH1*(m+1)/n
				la2 = r-dh x2 y2  +  mainH1*m/n
				lb2 = r-dh x2 y2  +  mainH1*(m+1)/n
			in
				-- Resulting triangles: 
				[((x1,y1,la1), (x2,y2,la2), (x2,y2,lb2)), 
				 ((x1,y1,la1), (x2,y2,lb2), (x1,y1,lb1)) ]
		-- Get a contour polyline for obj2, turn it into a list of segments
		segs = concat $ map segify $ symbolicGetContour res obj2
		-- Create sides for the main body of our object = segs × (r,h-r)
		-- Many layers...
		side_tris = concat $
			[concat $ map (\(a,b) -> segToSide m a b) segs | m <- [0.. n-1] ]
		-- Triangles that fill the contour. Make sure the mesh is at least (res/5) fine.
		-- --res/5 because xyres won't always match up with normal res and we need to compensate.
		fill_tris = {-divideMeshTo (res/5) $-} symbolicGetContourMesh res obj2
		-- The bottom. Use dh to determine the z coordinates
		bottom_tris = [((a1,a2,r-dh a1 a2), (b1,b2,r - dh b1 b2), (c1,c2,r - dh c1 c2)) 
				| ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
		-- Same idea at the top.
		top_tris = [((a1,a2,h-r+dh a1 a2), (b1,b2,h-r+dh b1 b2), (c1,c2,h-r+dh c1 c2)) 
				| ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
		-- Mesh modifiers in individual components
		fx :: ℝ3 -> ℝ
		fx (x,y,z) = fst $ mod z (x,y)
		fy :: ℝ3 -> ℝ
		fy (x,y,z) = snd $ mod z (x,y)
		-- function to transform a triangle
		transformTriangle :: (ℝ3,ℝ3,ℝ3) -> (ℝ3,ℝ3,ℝ3)
		transformTriangle (a@(_,_,z1), b@(_,_,z2), c@(_,_,z3)) = 
			((fx a, fy a, z1), (fx b, fy b, z2), (fx c, fy c, z3))

	in
		map transformTriangle (side_tris ++ bottom_tris ++ top_tris)

-- If all that fails, coerce and apply marching cubes :(
-- (rebound is for being safe about the bounding box --
--  it slightly streches it to make sure nothing will 
--  have problems because it is right at the edge )
symbolicGetMesh res  obj = 
	case rebound3 (coerceSymbolic3 obj) of
		(obj, (a,b)) -> getMesh a b res obj 

