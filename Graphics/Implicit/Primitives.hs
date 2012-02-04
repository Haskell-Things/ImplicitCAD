-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives (
	sphere,
	circle,
	cylinder, cylinderC, cylinder2, cylinder2C,
	rect3R, rectR,
	regularPolygon,
	polygonR,
	zsurface
) where

-- Some type definitions :)
import Graphics.Implicit.Definitions

-- Most of the functions we're exporting come from here
-- They are methods of classes.
import Graphics.Implicit.Primitives.Definitions

-- These files implement the classes for types
-- corresponding to the name of the file.
import Graphics.Implicit.Primitives.Obj2
import Graphics.Implicit.Primitives.Obj3
import Graphics.Implicit.Primitives.BoxedObj2
import Graphics.Implicit.Primitives.BoxedObj3
import Graphics.Implicit.Primitives.SymbolicObj2
import Graphics.Implicit.Primitives.SymbolicObj3

-- We export a few functions modified by operations
-- for users conveniences...
import Graphics.Implicit.Operations

-- If you are confused as to how these functions work, please refer to
-- http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

cylinder :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> obj    -- ^ Resulting cylinder
cylinder r h = cylinder2 r r h

cylinderC :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> obj    -- ^ Resulting cylinder
cylinderC r h = translate (0,0,-h/2.0) $ cylinder r h


cylinder2C :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Second radius of the cylinder
	-> ℝ      -- ^ Height of the cylinder
	-> obj    -- ^ Resulting cylinder
cylinder2C r1 r2 h = translate (0,0,-h/2.0) $ cylinder2 r1 r2 h



regularPolygon ::
	ℕ       -- ^ number of sides
	-> ℝ    -- ^ radius
	-> Obj2 -- ^ resulting regular polygon
regularPolygon sides r = polygonR 0 [ (r*cos(2*pi*m/sidesr), r*sin(2*pi*m/sidesr)) | m <- [0.. sidesr -1]]
	where sidesr = fromIntegral sides


zsurface ::
	(ℝ2 -> ℝ) -- ^ Description of the height of the surface
	-> Obj3   -- ^ Resulting 3D object
zsurface f = \(x,y,z) -> f (x,y) - z


-- This function is commented out because it doesn't obey the magnitude requirement.
-- Refer to blog post.
-- It needs to be fixed at some point, but the math is somewhat non-trivial.
--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b
--    | a < b = \(x,y) -> sqrt ((b/a*x)**2 + y**2) - a
--    | otherwise = \(x,y) -> sqrt (x**2 + (a/b*y)**2) - b
