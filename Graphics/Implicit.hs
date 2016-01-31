-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

{- The purpose of this file is to pass on the functionality we want
   to be accessible to an end user who is compiling objects using
   this haskell library. -}

-- MAYBEFIXME: impliment slice operation , regularPolygon and zsurface primitives.

module Graphics.Implicit(
    -- Operations
    translate,
    scale,
    complement,
    union,  intersect,  difference,
    unionR, intersectR, differenceR,
    shell,
    extrudeR,
    extrudeOnEdgeOf,
    -- Primitives
    sphere,
    rect3R,
    circle,
    cylinder,
    cylinder2,
    rectR,
    polygon,
    -- Export
    writeSVG,
    writeSTL,
    writeBinSTL,
    writeOBJ,
    writeTHREEJS,
    writeSCAD2,
    writeSCAD3,
    writeGCodeHacklabLaser,
    writePNG2,
    writePNG3,
    runOpenscad,
    implicit,
    SymbolicObj2,
    SymbolicObj3
) where

-- Let's be explicit about where things come from :)

-- The primitive objects, and functions for manipulating them.
import Graphics.Implicit.Primitives (translate, scale, complement, union, intersect, difference, unionR, intersectR, differenceR, shell, extrudeR, extrudeOnEdgeOf, sphere, rect3R, circle, cylinder, cylinder2, rectR, polygon, implicit)

-- The Extended OpenScad interpreter
import Graphics.Implicit.ExtOpenScad (runOpenscad)

-- Export functions, for writing files based on the result of operations on primitives
import qualified Graphics.Implicit.Export as Export (writeSVG, writeSTL, writeBinSTL, writeOBJ, writeSCAD2, writeSCAD3, writeTHREEJS, writeGCodeHacklabLaser, writePNG)

-- Datatypes/classes defining the world, or part of the world.
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ)


-- We want Export to be a bit less polymorphic
-- (so that types will collapse nicely)
writeSVG    = Export.writeSVG   :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writeSTL    = Export.writeSTL   :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeBinSTL = Export.writeBinSTL   :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeOBJ    = Export.writeOBJ   :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeSCAD2  = Export.writeSCAD2 :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writeSCAD3  = Export.writeSCAD3 :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeTHREEJS = Export.writeTHREEJS :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeGCodeHacklabLaser = Export.writeGCodeHacklabLaser :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writePNG2 = Export.writePNG :: ℝ -> FilePath -> SymbolicObj2  -> IO ()
writePNG3 = Export.writePNG :: ℝ -> FilePath -> SymbolicObj3  -> IO ()

