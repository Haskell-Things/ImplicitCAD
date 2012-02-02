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

import Graphics.Implicit.Definitions
import Graphics.Implicit.Operations

import Graphics.Implicit.Primitives.Definitions
import Graphics.Implicit.Primitives.Obj2
import Graphics.Implicit.Primitives.Obj3
import Graphics.Implicit.Primitives.BoxedObj2
import Graphics.Implicit.Primitives.BoxedObj3
import Graphics.Implicit.Primitives.SymbolicObj2
import Graphics.Implicit.Primitives.SymbolicObj3

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









-- This function is commented out because it doesn't obey the magnitude requirement.
-- Refer to blog post.
-- It needs to be fixed at some point, but the math is somewhat non-trivial.
--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b
--    | a < b = \(x,y) -> sqrt ((b/a*x)**2 + y**2) - a
--    | otherwise = \(x,y) -> sqrt (x**2 + (a/b*y)**2) - b


regularPolygon ::
	ℕ       -- ^ number of sides
	-> ℝ    -- ^ radius
	-> Obj2 -- ^ resulting regular polygon
regularPolygon sides r = let sidesr = fromIntegral sides in
	\(x,y) -> maximum [ x*cos(2*pi*m/sidesr) + y*sin(2*pi*m/sidesr) | m <- [0.. sidesr -1]] - r


zsurface ::
	(ℝ2 -> ℝ) -- ^ Description of the height of the surface
	-> Obj3   -- ^ Resulting 3D object
zsurface f = \(x,y,z) -> f (x,y) - z


