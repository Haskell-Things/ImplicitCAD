-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives (
	sphere,
	cube, cubeC, cubeV, cubeVC,
	circle,
	cylinder, cylinderC, cylinder2, cylinder2C,
	square, squareC, squareV, squareVC,
	torus,
	regularPolygon,
	polygon,
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


cube :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ          -- ^ Width of the cube
	 -> obj    -- ^ Resuting cube - (0,0,0) is bottom left...
cube l = cubeV (l,l,l)

cubeC :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ          -- ^ Width of the cube
	 -> obj    -- ^ Resuting centered cube
cubeC l = translate (-l/2.0, -l/2.0, -l/2.0) $ cube l

cubeVC :: (PrimitiveSupporter3 obj, BasicObj obj ℝ3) =>
	ℝ3         -- ^ Dimensions of the cube
	 -> obj    -- ^ Resuting cube - (0,0,0) is bottom left...
cubeVC (dx, dy, dz) = translate (-dx/2.0, -dy/2.0, -dz/2.0) $ cubeV (dx, dy, dz)


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

square :: (PrimitiveSupporter2 obj, BasicObj obj ℝ2) =>
	ℝ        -- ^ Width of the square
	-> obj   -- ^ Resulting square (bottom corner = (0,0) )
square l = squareV (l,l)

squareC :: (PrimitiveSupporter2 obj, BasicObj obj ℝ2) =>
	ℝ        -- ^ Width of the square
	-> obj   -- ^ Resulting square (centered on (0,0))
squareC l = squareVC (l,l)


squareVC :: (PrimitiveSupporter2 obj, BasicObj obj ℝ2) =>
	ℝ2        -- ^ Width of the square
	-> obj    -- ^ Resulting square
squareVC (dx,dy) = translate (-dx/2.0, -dy/2.0) $ squareV (dx,dy)







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


