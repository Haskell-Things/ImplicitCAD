-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{- The purpose of this file is to pass on the functionality we want
   to be accessible to an end user who is compiling objects using
   this haskell library. -}

module Graphics.Implicit (
  -- * Types
  W.ℝ,
  W.ℝ2,
  W.ℝ3,
  SymbolicObj2 (),
  SymbolicObj3 (),
  W.ExtrudeRMScale(C1, C2, Fn),

  -- * Shared operations
  P.Object (),
  P.translate,
  P.scale,
  P.mirror,
  P.complement,
  P.union,
  P.unionR,
  P.intersect,
  P.intersectR,
  P.difference,
  P.differenceR,
  P.implicit,
  P.shell,
  P.outset,
  P.emptySpace,
  P.fullSpace,

  -- * 2D primitive shapes
  P.squareR,
  P.rectR,
  P.circle,
  P.polygonR,

  -- * 2D operations
  P.rotate,
  P.pack2,

  -- * 3D primitive shapes
  P.cubeR,
  P.rect3R,
  P.sphere,
  P.cylinder,
  P.cylinder2,

  -- * 3D operations
  P.rotate3,
  P.rotate3V,
  P.pack3,

  -- * Extrusions into 3D
  P.extrudeR,
  P.extrudeRM,
  P.extrudeOnEdgeOf,
  P.rotateExtrude,

  -- * OpenScad support
  E.runOpenscad,

  -- * 2D exporters
  writeSVG,
  writePNG2,
  writeDXF2,
  writeSCAD2,
  writeGCodeHacklabLaser,

  -- * 3D exporters
  writeSTL,
  writeBinSTL,
  writeOBJ,
  writeTHREEJS,
  writeSCAD3,
  writePNG3
) where

import Prelude
import Linear(V2(..), V3(..))

-- The primitive objects, and functions for manipulating them.
-- MAYBEFIXME: impliment slice operation, regularPolygon and zsurface primitives.
import Graphics.Implicit.Primitives as P (rectR, rect3R, translate, scale, mirror, complement, union, intersect, difference, unionR, intersectR, differenceR, shell, extrudeR, extrudeRM, extrudeOnEdgeOf, sphere, cubeR, circle, cylinder, cylinder2, squareR, polygonR, rotateExtrude, rotate3, rotate3V, pack3, rotate, pack2, implicit, fullSpace, emptySpace, outset, Object)

-- The Extended OpenScad interpreter.
import Graphics.Implicit.ExtOpenScad as E (runOpenscad)

-- typesclasses and types defining the world, or part of the world.
import Graphics.Implicit.Definitions as W (ℝ, ℝ2, ℝ3, SymbolicObj2, SymbolicObj3, ExtrudeRMScale(C1, C2, Fn))

-- Functions for writing files based on the result of operations on primitives.
import qualified Graphics.Implicit.Export as Export (writeSVG, writeDXF2, writeSTL, writeBinSTL, writeOBJ, writeSCAD2, writeSCAD3, writeTHREEJS, writeGCodeHacklabLaser, writePNG)

-- We want Export to be a bit less polymorphic
-- (so that types will collapse nicely)

writeSVG
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-2)/ more time.
    -> FilePath
    -> SymbolicObj2
    -> IO ()
writeSVG = Export.writeSVG

writeDXF2
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-2)/ more time.
    -> FilePath
    -> SymbolicObj2
    -> IO ()
writeDXF2 = Export.writeDXF2

writeSTL
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writeSTL = Export.writeSTL

writeBinSTL
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writeBinSTL = Export.writeBinSTL

writeOBJ
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writeOBJ = Export.writeOBJ

writeSCAD2
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-2)/ more time.
    -> FilePath
    -> SymbolicObj2
    -> IO ()
writeSCAD2 = Export.writeSCAD2

writeSCAD3
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writeSCAD3 = Export.writeSCAD3

writeTHREEJS
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writeTHREEJS = Export.writeTHREEJS

writeGCodeHacklabLaser
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-2)/ more time.
    -> FilePath
    -> SymbolicObj2
    -> IO ()
writeGCodeHacklabLaser = Export.writeGCodeHacklabLaser

writePNG2
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-2)/ more time.
    -> FilePath
    -> SymbolicObj2
    -> IO ()
writePNG2 = Export.writePNG


-- | Export a PNG of the 'SymbolicObj3'. The projection is with a front-facing
-- camera, so the coordinate system is @(left to right, front to back, down to
-- up)@.
writePNG3
    :: ℝ  -- ^ Rendering resolution, in millimeters. Smaller values produce
          -- exports more faithful to the implicit model, at the expense of
          -- taking /O(n^-3)/ more time.
    -> FilePath
    -> SymbolicObj3
    -> IO ()
writePNG3 = Export.writePNG


main = writeSTL 2 "/tmp/yo.stl"  $ translate (V3 0.0 (-27.5) 0.0) (union [translate (V3 62.0 0.0 33.0) (union [translate (V3 (-124.0) 0.0 0.0) (cubeR 0.0 False (V3 124.0 124.0 2.0)),translate (V3 (-56.0) 62.0 (-33.0)) (union [translate (V3 0.0 0.0 35.0) (union [translate (V3 0.0 0.0 (-17.5)) (difference (translate (V3 0.0 0.0 (-17.5)) (cylinder 42.0 35.0)) [translate (V3 0.0 0.0 (-15.5)) (cylinder 40.0 33.0)]),union [translate (V3 0.0 0.0 (-2.0)) (cylinder 48.0 2.0),emptySpace]]),emptySpace])]),translate (V3 (-3.5) (-50.0) 0.0) (union [translate (V3 0.0 (-9.5) 0.0) (union [translate (V3 0.0 0.0 8.5) (rotate3 (V3 3.141592653589793 0.0 0.0) (translate (V3 (-62.0) (-9.5) (-8.5)) (cubeR 0.0 False (V3 124.0 19.0 17.0)))),union [translate (V3 61.0 0.0 0.0) (union [translate (V3 0.0 (-4.0) 0.0) (cubeR 0.0 False (V3 8.0 8.0 2.0)),translate (V3 (-2.0) (-4.0) 0.0) (cubeR 0.0 False (V3 2.0 8.0 8.0))]),emptySpace]]),translate (V3 0.0 52.0 0.0) (union [translate (V3 (-52.0) (-54.0) 0.0) (cubeR 0.0 False (V3 104.0 54.0 17.0)),translate (V3 0.0 (-2.0) 0.0) (difference (extrudeR 0.0 (polygonR 0.0 [V2 (-48.0) 0.0,V2 48.0 0.0,V2 0.0 60.0]) 17.0) [translate (V3 0.0 0.0 2.0) (extrudeR 0.0 (polygonR 0.0 [V2 (-46.0) 0.0,V2 46.0 0.0,V2 0.0 58.0]) 13.0)])])])])

