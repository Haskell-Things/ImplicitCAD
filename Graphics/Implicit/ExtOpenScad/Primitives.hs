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

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.Primitives as Prim
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Data.Map (Map, lookup)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr

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
	r      :: ℝ    <- argument "r"      `defaultTo` 1
	h      :: ℝ    <- argument "h"      `defaultTo` 1
	r1     :: ℝ    <- argument "r1"     `defaultTo` 1
	r2     :: ℝ    <- argument "r2"     `defaultTo` 1
	center :: Bool <- argument "center" `defaultTo` False
	if r1 == 1 && r2 == 1
		then if center
			then addObj3 $ Prim.cylinderC r h
			else addObj3 $ Prim.cylinder  r h
		else if center
			then addObj3 $ Prim.cylinder2C r1 r2 h
			else addObj3 $ Prim.cylinder2  r1 r2 h


circle = moduleWithoutSuite "circle" $ do
	r  :: ℝ <- argument "r"
	fn :: ℕ <- argument "$fn" `defaultTo` (-1)
	if fn < 3
		then addObj2 $ Prim.circle r
		else addObj2 $ Prim.polygonR 0 [(r*cos θ, r*sin θ )| θ <- [2*pi*n/fromIntegral fn | n <- [0.0 .. fromIntegral fn - 1.0]]]



polygon = moduleWithoutSuite "polygon" $ do
	points :: [ℝ2] <-  argument "points" 
	                    `doc` "vertices of the polygon"
	pathes :: [ℕ ]  <- argument "pathes" 
	                    `doc` "order to go through vertices; ignored for now"
	                    `defaultTo` []
	case pathes of
		[] -> addObj2 $ Prim.polygonR 0 points
		_ -> noChange;



