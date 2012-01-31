-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

{- The sole purpose of this file it to pass on the
   functionality we want to be accessible to the end user. -}

module Graphics.Implicit(
	-- Operations
	translate,
	scale,
	complement,
	union,  intersect,  difference,
	unionR, intersectR, differenceR,
	shell,
	--slice,
	extrudeR,
	extrudeOnEdgeOf,
	-- Primitives
	sphere,
	cube,
	circle,
	cylinder,
	square,
	--regularPolygon,
	--zsurface,
	polygon,
	-- Export
	writeSVG,
	writeSTL,
	runOpenscad
) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.Primitives as S
import Graphics.Implicit.Operations
import qualified Graphics.Implicit.Export as Export
import Graphics.Implicit.ExtOpenScad

-- The versions of objects that should be used by default.
-- Import Graphics.Implicit.Primitives to override
type DObj3 = SymbolicObj3
type DObj2 = SymbolicObj2

writeSTL :: ℝ -> String -> DObj3 -> IO()
writeSTL = Export.writeSTL
writeSVG :: ℝ -> String -> DObj2 -> IO()
writeSVG = Export.writeSVG


sphere ::
	ℝ           -- ^ Radius of the sphere
	-> DObj3    -- ^ Resulting sphere
sphere = S.sphere
cubeV ::
	ℝ3          -- ^ Dimensions of the cube
	 -> DObj3   -- ^ Resuting cube - (0,0,0) is bottom left...
cubeV = S.cubeV
cylinder2 ::
	ℝ           -- ^ Radius of the cylinder	
	-> ℝ        -- ^ Second radius of the cylinder
	-> ℝ        -- ^ Height of the cylinder
	-> DObj3    -- ^ Resulting cylinder
cylinder2 = S.cylinder2
torus ::
	ℝ           -- ^ radius of the rotated circle of a torus
	-> ℝ        -- ^ radius of the circle rotationaly extruded on of a torus
	-> DObj3    -- ^ resulting torus
torus = S.torus


circle ::
	ℝ          -- ^ radius of the circle
	-> DObj2   -- ^ resulting circle
circle = S.circle
squareV ::
	ℝ2         -- ^ (x width, y width)
	-> DObj2   -- ^ Resulting square (bottom right = (0,0) )
squareV = S.squareV
polygon ::
	[ℝ2]      -- ^ Verticies of the polygon
	 -> DObj2   -- ^ Resulting polygon
polygon = S.polygon



cube :: 
	ℝ          -- ^ Width of the cube
	 -> DObj3    -- ^ Resuting cube - (0,0,0) is bottom left...
cube l = cubeV (l,l,l)

cubeC :: 
	ℝ          -- ^ Width of the cube
	 -> DObj3   -- ^ Resuting centered cube
cubeC l = translate (-l/2.0, -l/2.0, -l/2.0) $ cube l

cubeVC ::
	ℝ3         -- ^ Dimensions of the cube
	 -> DObj3    -- ^ Resuting cube - (0,0,0) is bottom left...
cubeVC (dx, dy, dz) = translate (-dx/2.0, -dy/2.0, -dz/2.0) $ cubeV (dx, dy, dz)


cylinder ::
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> DObj3   -- ^ Resulting cylinder
cylinder r h = cylinder2 r r h

cylinderC :: 
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> DObj3    -- ^ Resulting cylinder
cylinderC r h = translate (0,0,-h/2.0) $ cylinder r h


cylinder2C :: 
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Second radius of the cylinder
	-> ℝ      -- ^ Height of the cylinder
	-> DObj3   -- ^ Resulting cylinder
cylinder2C r1 r2 h = translate (0,0,-h/2.0) $ cylinder2 r1 r2 h









-- This function is commented out because it doesn't obey the magnitude requirement.
-- Refer to blog post.
-- It needs to be fixed at some point, but the math is somewhat non-trivial.
--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b
--    | a < b = \(x,y) -> sqrt ((b/a*x)**2 + y**2) - a
--    | otherwise = \(x,y) -> sqrt (x**2 + (a/b*y)**2) - b

square :: 
	ℝ        -- ^ Width of the square
	-> DObj2   -- ^ Resulting square (bottom corner = (0,0) )
square l = squareV (l,l)

squareC :: 
	ℝ        -- ^ Width of the square
	-> DObj2   -- ^ Resulting square (centered on (0,0))
squareC l = squareVC (l,l)


squareVC ::
	ℝ2        -- ^ Width of the square
	-> DObj2    -- ^ Resulting square
squareVC (dx,dy) = translate (-dx/2.0, -dy/2.0) $ squareV (dx,dy)


