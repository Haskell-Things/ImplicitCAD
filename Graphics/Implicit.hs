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
	rect3R,
	circle,
	cylinder,
	rectR,
	--regularPolygon,
	--zsurface,
	polygon,
	-- Export
	writeSVG,
	writeSTL,
	runOpenscad
) where

-- Let's be explicit about where things come from :)
import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, SymbolicObj2, SymbolicObj3)
import qualified Graphics.Implicit.Primitives as Prim
import qualified Graphics.Implicit.Export as Export
import Graphics.Implicit.ExtOpenScad (runOpenscad)
import Graphics.Implicit.Operations 
	(translate, scale, complement, 
	 union,  intersect,  difference,
	 unionR, intersectR, differenceR,
	 extrudeR, extrudeOnEdgeOf, shell)

-- The versions of objects that should be used by default.
-- Import Graphics.Implicit.Primitives to override

type DObj3 = SymbolicObj3
type DObj2 = SymbolicObj2

-- We're going to force some of the types to be less flexible 
-- than they are for ease of use for the end user...

writeSTL ::
	ℝ           -- ^ Resolution
	-> FilePath -- ^ STL file to write to
	-> DObj3    -- ^ 3D object to write
	-> IO()     -- ^ Writing action!

writeSTL = Export.writeSTL

writeSVG :: 
	ℝ           -- ^ Resolution
	-> FilePath -- ^ SVG File to be written to
	-> DObj2    -- ^ 2D object to write
	-> IO()     -- ^ Writing action!

writeSVG = Export.writeSVG


sphere ::
	ℝ           -- ^ Radius of the sphere
	-> DObj3    -- ^ Resulting sphere
sphere = Prim.sphere
rect3R ::
	ℝ           -- ^ Radius of roudning
	-> ℝ3       -- ^ bot-left-out corner
	-> ℝ3       -- ^ top-right-in corner
	 -> DObj3   -- ^ Resuting 3D rect
rect3R = Prim.rect3R

cylinder2 ::
	ℝ           -- ^ Radius of the cylinder	
	-> ℝ        -- ^ Second radius of the cylinder
	-> ℝ        -- ^ Height of the cylinder
	-> DObj3    -- ^ Resulting cylinder
cylinder2 = Prim.cylinder2

circle ::
	ℝ          -- ^ radius of the circle
	-> DObj2   -- ^ resulting circle
circle = Prim.circle

rectR ::
	ℝ          -- ^ Radius of rounding
	-> ℝ2      -- ^ (x1, y1)
	-> ℝ2      -- ^ (x2 ,y2)
	-> DObj2   -- ^ rect between (x1,y1) and (x2,y2)
rectR = Prim.rectR

polygon ::
	[ℝ2]      -- ^ Verticies of the polygon
	 -> DObj2   -- ^ Resulting polygon
polygon = Prim.polygonR 0



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

