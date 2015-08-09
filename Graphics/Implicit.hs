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
import Graphics.Implicit.Primitives
import Graphics.Implicit.ExtOpenScad (runOpenscad)
import qualified Graphics.Implicit.Export as Export
import Graphics.Implicit.Definitions

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

