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
	cylinder2,
	rectR,
	--regularPolygon,
	--zsurface,
	polygon,
	-- Export
	writeSVG,
	writeSTL,
	writeOBJ,
	runOpenscad
) where

-- Let's be explicit about where things come from :)
import Graphics.Implicit.Primitives
import Graphics.Implicit.ExtOpenScad (runOpenscad)
import Graphics.Implicit.Export


