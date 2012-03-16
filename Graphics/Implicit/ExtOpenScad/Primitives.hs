-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

-- This file provides primitive objects for the openscad parser.
-- The code is fairly straightforward; an explanation of how 
-- the first one works is provided.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Graphics.Implicit.ExtOpenScad.Primitives (primitives) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Graphics.Implicit.ExtOpenScad.Util.Computation

import qualified Graphics.Implicit.Primitives as Prim
import Data.Maybe (fromMaybe)

primitives :: [(String, [ComputationStateModifier] ->  ArgParser ComputationStateModifier)]
primitives = [ sphere, cube, square, cylinder, circle, polygon, union, difference, intersect, translate, scale, rotate, extrude, pack, shell ]

moduleWithSuite name modArgMapper = (name, modArgMapper)
moduleWithoutSuite name modArgMapper = (name, \suite -> modArgMapper)


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

	-- examples
	example "cube(size = [2,3,4], center = true, r = 0.5);"
	example "cube(4);"

	-- arguments
	size   :: Any  <- argument "size"
	                    `doc` "cube size"
	center :: Bool <- argument "center" 
	                    `doc` "should center?"  
	                    `defaultTo` False
	r      :: ℝ    <- argument "r"
	                    `doc` "radius of rounding" 
	                    `defaultTo` 0

	-- Tests
	test "cube(4);"
		`eulerCharacteristic` 2
	test "cube(size=[2,3,4]);"
		`eulerCharacteristic` 2

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

	-- examples 
	example "square(size = [3,4], center = true, r = 0.5);"
	example "square(4);"

	-- arguments
	size   :: Any  <- argument "size"
	                    `doc`  "square size"
	center :: Bool <- argument "center" 
	                    `doc` "should center?"  
	                    `defaultTo` False
	r      :: ℝ    <- argument "r"
	                    `doc` "radius of rounding" 
	                    `defaultTo` 0

	-- Tests
	test "square(2);"
		`eulerCharacteristic` 0
	test "square(size=[2,3]);"
		`eulerCharacteristic` 0

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

cylinder = moduleWithoutSuite "cylinder" $ do

	example "cylinder(r=10, h=30, center=true);"
	example "cylinder(r1=4, r2=6, h=10);"
	example	"cylinder(r=5, h=10, $fn = 6);"

	-- arguments
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

	-- Tests
	test "cylinder(r=10, h=30, center=true);"
		`eulerCharacteristic` 0
	test "cylinder(r=5, h=10, $fn = 6);"
		`eulerCharacteristic` 0

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
	
	example "circle(r=10); // circle"
	example "circle(r=5, $fn=6); //hexagon"

	-- Arguments
	r  :: ℝ <- argument "r"
		`doc` "radius of the circle"
	fn :: ℕ <- argument "$fn" 
		`doc` "if defined, makes a regular polygon with n sides instead of a circle"
		`defaultTo` (-1)

	test "circle(r=10);"
		`eulerCharacteristic` 0

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




union = moduleWithSuite "union" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.unionR r) (Prim.unionR r)
		else getAndCompressSuiteObjs suite Prim.union Prim.union

intersect = moduleWithSuite "intersection" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.intersectR r) (Prim.intersectR r)
		else getAndCompressSuiteObjs suite Prim.intersect Prim.intersect

difference = moduleWithSuite "difference" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.differenceR r) (Prim.differenceR r)
		else getAndCompressSuiteObjs suite Prim.difference Prim.difference

translate = moduleWithSuite "translate" $ \suite -> do
	v <- argument "v"
	caseOType v $
		       ( \(x,y,z)-> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,z)) 
		) <||> ( \(x,y) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,0.0)) 
		) <||> ( \ x -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,0.0) ) (Prim.translate (x,0.0,0.0))
		) <||> (\ _  -> noChange)

deg2rad x = x / 180.0 * pi

-- This is mostly insane
rotate = moduleWithSuite "rotate" $ \suite -> do
	a <- argument "a"
	caseOType a $
		       ( \xy  ->
			getAndTransformSuiteObjs suite (Prim.rotate $ deg2rad xy ) (Prim.rotate3 (deg2rad xy, 0, 0) )
		) <||> ( \(yz,xy,xz) ->
			getAndTransformSuiteObjs suite (Prim.rotate $ deg2rad xy ) (Prim.rotate3 (deg2rad yz, deg2rad xz, deg2rad xy) )
		) <||> ( \(yz,xz) ->
			getAndTransformSuiteObjs suite (id ) (Prim.rotate3 (deg2rad yz, deg2rad xz, 0))
		) <||> ( \_  -> noChange )


scale = moduleWithSuite "scale" $ \suite -> do
	v <- argument "v"
	case v of
		{-OList ((ONum x):(ONum y):(ONum z):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,z))
		OList ((ONum x):(ONum y):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,0.0))
		OList ((ONum x):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,0.0) ) (Prim.translate (x,0.0,0.0)-}
		ONum s ->
			getAndTransformSuiteObjs suite (Prim.scale s) (Prim.scale s)

extrude = moduleWithSuite "linear_extrude" $ \suite -> do
	height :: ℝ   <- argument "height"
	center :: Bool<- argument "center" `defaultTo` False
	twist  :: Any <- argument "twist"  `defaultTo` (ONum 0)
	r      :: ℝ   <- argument "r"      `defaultTo` 0
	let
		degRotate = (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ))) . (*(2*pi/360))
		shiftAsNeeded =
			if center
			then Prim.translate (0,0,-height/2.0)
			else id
	caseOType twist $
		(\ (rot :: ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ if rot == 0 
					then Prim.extrudeR    r                               obj height
					else Prim.extrudeRMod r (degRotate . (*(rot/height))) obj height
		) <||> (\ (rotf :: ℝ -> Maybe ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ Prim.extrudeRMod r 
					(degRotate . (fromMaybe 0) . rotf) obj height
		) <||> (\_ -> noChange)

{-rotateExtrudeStatement = moduleWithSuite "rotate_extrude" $ \suite -> do
	h <- realArgument "h"
	center <- boolArgumentWithDefault "center" False
	twist <- realArgumentWithDefault 0.0
	r <- realArgumentWithDefault "r" 0.0
	getAndModUpObj2s suite (\obj -> Prim.extrudeRMod r (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ)) )  obj h) 
-}

shell = moduleWithSuite "shell" $ \suite -> do
	w :: ℝ <- argument "w"
	getAndTransformSuiteObjs suite (Prim.shell w) (Prim.shell w)

-- Not a perenant solution! Breaks if can't pack.
pack = moduleWithSuite "pack" $ \suite -> do
	size :: ℝ2 <- argument "size"
	sep  :: ℝ  <- argument "sep"
	let
		pack2 objs = case Prim.pack2 size sep objs of
			Just a -> a
		pack3 objs = case Prim.pack3 size sep objs of
			Just a -> a
	getAndCompressSuiteObjs  suite pack2 pack3


