-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

-- This file provides primitive objects for the openscad parser.
-- The code is fairly straightforward; an explanation of how 
-- the first one works is provided.

-- Note: Primitives must be added to the computationStatement parser in
-- Graphics.Implicit.ExtOpenScad.Statements to have any effect!!!


{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Graphics.Implicit.ExtOpenScad.Primitives where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util

import qualified Graphics.Implicit.Primitives as Prim

import Data.Map (Map, lookup)

primitives = [sphere, cube, square, cylinder, circle, polygon]

-- **Exmaple of implementing a module**
-- sphere is a module without a suite named sphere,
-- this means that the parser will look for this like
--       sphere(args...);
sphere = moduleWithoutSuite "sphere" $ do
	-- What are the arguments?
	-- The radius, r, which is a (real) number.
	-- Because we don't provide a default, this ends right
	-- here if it doesn't get a suitable argument!
	r :: ℝ <- argument "r" 
	            `doc` "radius of the sphere"
	-- So what does this module do?
	-- It adds a 3D object, a sphere of radius r,
	-- using the sphere implementation in Prim
	-- (Graphics.Implicit.Primitives)
	addObj3 $ Prim.sphere r

cube = moduleWithoutSuite "cube" $ do
	-- arguments 
	-- eg.   cube(size = [2,3,4], center = true, r = 0.5)
	-- eg.   cube(4)
	size   :: Any  <- argument "size"
	                    `doc` "cube size"
	center :: Bool <- argument "center" 
	                    `doc` "should center?"  
	                    `defaultTo` False
	r      :: ℝ    <- argument "r"
	                    `doc` "radius of rounding" 
	                    `defaultTo` 0
	-- A helper function for making rect3's accounting for centerdness
	let rect3 x y z = 
		if center  
		then Prim.rect3R r (-x/2, -y/2, -z/2) (x/2, y/2, z/2)
		else Prim.rect3R r (0, 0, 0)  (x, y, z)
	-- caseOType matches depending on whether size can be coerced into
	-- the right object. See Graphics.Implicit.ExtOpenScad.Util
	-- Entries must be joined with the operator <||>
	-- Final entry must be fall through.
	caseOType size $
		     ( \(x,y,z) -> addObj3 $ rect3 x y z)
		<||> ( \w       -> addObj3 $ rect3 w w w)
		<||> ( \_       -> noChange )


square = moduleWithoutSuite "square" $ do
	-- arguments 
	-- eg.   square(size = [3,4], center = true, r = 0.5)
	-- eg.   square(4)
	size   :: Any  <- argument "size"
	                    `doc`  "square size"
	center :: Bool <- argument "center" 
	                    `doc` "should center?"  
	                    `defaultTo` False
	r      :: ℝ    <- argument "r"
	                    `doc` "radius of rounding" 
	                    `defaultTo` 0
	-- A helper function for making rect2's accounting for centerdness
	let rect x y = 
		if center  
		then Prim.rectR r (-x/2, -y/2) (x/2, y/2)
		else Prim.rectR r (  0,    0 ) ( x,   y )
	-- caseOType matches depending on whether size can be coerced into
	-- the right object. See Graphics.Implicit.ExtOpenScad.Util
	caseOType size $
		     (\(x,y) -> addObj2 $ rect x y)
		<||> (\w     -> addObj2 $ rect w w)
		<||> (\_     -> noChange)

-- What about $fn for regular n-gon prisms? This will break models..
cylinder = moduleWithoutSuite "cylinder" $ do
	-- arguments
	-- eg. cylinder(r=10, h=30, center=true);
	--     cylinder(r1=4, r2=6, h=10);
	--     cylinder(r=5, h=10, $fn = 6);
	r      :: ℝ    <- argument "r"
				`defaultTo` 1
				`doc` "radius of cylinder"
	h      :: ℝ    <- argument "h"
				`defaultTo` 1
				`doc` "height of cylinder"
	r1     :: ℝ    <- argument "r1"
				`defaultTo` 1
				`doc` "bottom radius; overrides r"
	r2     :: ℝ    <- argument "r2"
				`defaultTo` 1
				`doc` "top radius; overrides r"
	fn     :: ℕ    <- argument "$fn"
				`defaultTo` (-1)
				`doc` "number of sides, for making prisms"
	center :: Bool <- argument "center"
				`defaultTo` False
				`doc` "center cylinder with respect to z?"
	-- The result is a computation state modifier that adds a 3D object, 
	-- based on the args.
	addObj3 $ if r1 == 1 && r2 == 1
		then let
			obj2 = if fn  < 0 then Prim.circle r else Prim.polygonR 0 $
				let sides = fromIntegral fn 
				in [(r*cos θ, r*sin θ )| θ <- [2*pi*n/sides | n <- [0.0 .. sides - 1.0]]]
			obj3 = Prim.extrudeR 0 obj2 h
		in if center
			then Prim.translate (0,0,-h/2) obj3
			else obj3
		else if center
			then  Prim.translate (0,0,-h/2) $ Prim.cylinder2 r1 r2 h
			else Prim.cylinder2  r1 r2 h

circle = moduleWithoutSuite "circle" $ do
	-- arguments
	-- eg. circle(r=10); // circle
	--     circle(r=5, $fn=6); //hexagon
	r  :: ℝ <- argument "r"
		`doc` "radius of the circle"
	fn :: ℕ <- argument "$fn" 
		`doc` "if defined, makes a regular polygon with n sides instead of a circle"
		`defaultTo` (-1)
	if fn < 3
		then addObj2 $ Prim.circle r
		else addObj2 $ Prim.polygonR 0 $
			let sides = fromIntegral fn 
			in [(r*cos θ, r*sin θ )| θ <- [2*pi*n/sides | n <- [0.0 .. sides - 1.0]]]

polygon = moduleWithoutSuite "polygon" $ do
	points :: [ℝ2] <-  argument "points" 
	                    `doc` "vertices of the polygon"
	paths :: [ℕ ]  <- argument "paths" 
	                    `doc` "order to go through vertices; ignored for now"
	                    `defaultTo` []
	r      :: ℝ     <- argument "r"
	                    `doc` "rounding of the polygon corners; ignored for now"
	                    `defaultTo` 0
	case paths of
		[] -> addObj2 $ Prim.polygonR 0 points
		_ -> noChange;



