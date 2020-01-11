-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use real types in the type constraints.
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Implicit.Export (writeObject, formatObject, writeSVG, writeSTL, writeBinSTL, writeOBJ, writeTHREEJS, writeGCodeHacklabLaser, writeDXF2, writeSCAD2, writeSCAD3, writePNG) where

import Prelude (FilePath, IO, (.), ($))

-- The types of our objects (before rendering), and the type of the resolution to render with.
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ, Polyline, TriangleMesh, NormedTriangleMesh)

-- functions for outputing a file, and one of the types.
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import qualified Data.ByteString.Lazy as LBS (writeFile)

-- Import instances of DiscreteApproxable...
import Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable, discreteAprox)

-- Output file formats.
import qualified Graphics.Implicit.Export.PolylineFormats as PolylineFormats (svg, hacklabLaserGCode, dxf2)
import qualified Graphics.Implicit.Export.TriangleMeshFormats as TriangleMeshFormats (stl, binaryStl, jsTHREE)
import qualified Graphics.Implicit.Export.NormedTriangleMeshFormats as NormedTriangleMeshFormats (obj)
import qualified Graphics.Implicit.Export.SymbolicFormats as SymbolicFormats (scad2, scad3)
import qualified Codec.Picture as ImageFormatCodecs (DynamicImage, savePngImage)

-- | Write an object to a file with LazyText IO, using the given format writer function.
writeObject :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (aprox -> Text)  -- ^ File Format Writer (Function that formats)
    -> FilePath         -- ^ File Name
    -> obj              -- ^ Object to render
    -> IO ()            -- ^ Writing Action!
writeObject res formatWriter filename obj =
    let
        aprox = formatObject res formatWriter obj
    in LT.writeFile filename aprox

-- | Serialize an object using the given format writer, which takes the filename and writes to it..
writeObject' :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (FilePath -> aprox -> IO ())  -- ^ File Format writer
    -> FilePath         -- ^ File Name
    -> obj              -- ^ Object to render
    -> IO ()            -- ^ Writing Action!
writeObject' res formatWriter filename obj =
    formatWriter filename (discreteAprox res obj)

-- | Serialize an object using the given format writer. No file target is implied.
formatObject :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (aprox -> Text)  -- ^ File Format Writer (Function that formats)
    -> obj              -- ^ Object to render
    -> Text             -- ^ Resulting lazy ByteString
formatObject res formatWriter = formatWriter . discreteAprox res

writeSVG :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeSVG res = writeObject res PolylineFormats.svg

writeDXF2 :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeDXF2 res = writeObject res PolylineFormats.dxf2

writeSTL :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeSTL res = writeObject res TriangleMeshFormats.stl

writeBinSTL :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeBinSTL res file obj = LBS.writeFile file $ TriangleMeshFormats.binaryStl $ discreteAprox res obj

writeOBJ :: DiscreteAproxable obj NormedTriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeOBJ res = writeObject res NormedTriangleMeshFormats.obj

writeTHREEJS :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeTHREEJS res = writeObject res TriangleMeshFormats.jsTHREE

writeGCodeHacklabLaser :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeGCodeHacklabLaser res = writeObject res PolylineFormats.hacklabLaserGCode

writeSCAD3 :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeSCAD3 res filename obj = LT.writeFile filename $ SymbolicFormats.scad3 res obj

writeSCAD2 :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writeSCAD2 res filename obj = LT.writeFile filename $ SymbolicFormats.scad2 res obj

writePNG :: DiscreteAproxable obj ImageFormatCodecs.DynamicImage => ℝ -> FilePath -> obj -> IO ()
writePNG res = writeObject' res ImageFormatCodecs.savePngImage
