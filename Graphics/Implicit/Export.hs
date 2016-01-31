-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Graphics.Implicit.Export where

-- The types of our objects (before rendering), and the type of the resolution to render with.
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ)

-- The functions for writing our output, as well as a type used.
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import qualified Data.ByteString.Lazy as LBS (writeFile)

-- FIXME: document these.
import Graphics.Implicit.Export.Definitions (DiscreteAproxable, discreteAprox)

-- Import instances of DiscreteApproxable...
import Graphics.Implicit.Export.SymbolicObj2 ()

import Graphics.Implicit.Export.SymbolicObj3 ()
import Graphics.Implicit.Export.RayTrace ()

-- Object formats
import qualified Graphics.Implicit.Export.PolylineFormats as PolylineFormats
import qualified Graphics.Implicit.Export.TriangleMeshFormats as TriangleMeshFormats
import qualified Graphics.Implicit.Export.NormedTriangleMeshFormats as NormedTriangleMeshFormats
import qualified Graphics.Implicit.Export.SymbolicFormats as SymbolicFormats

import qualified Codec.Picture as ImageFormatCodecs

-- Write an object using the given format function.
writeObject :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (aprox -> Text)  -- ^ File Format (Function that formats)
    -> FilePath         -- ^ File Name
    -> obj              -- ^ Object to render
    -> IO ()            -- ^ Writing Action!
writeObject res format filename obj =
    let aprox = formatObject res format obj
    in LT.writeFile filename aprox

-- Write an object using the given format writer.
writeObject' :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (FilePath -> aprox -> IO ())  -- ^ File Format writer
    -> FilePath         -- ^ File Name
    -> obj              -- ^ Object to render
    -> IO ()            -- ^ Writing Action!
writeObject' res formatWriter filename obj =
    let aprox = discreteAprox res obj
    in formatWriter filename aprox

formatObject :: (DiscreteAproxable obj aprox)
    => ℝ                -- ^ Resolution
    -> (aprox -> Text)  -- ^ File Format (Function that formats)
    -> obj              -- ^ Object to render
    -> Text             -- ^ Resulting lazy ByteString
formatObject res format = format . discreteAprox res

writeSVG res = writeObject res PolylineFormats.svg

writeSTL res = writeObject res TriangleMeshFormats.stl

writeBinSTL res file obj = LBS.writeFile file $ TriangleMeshFormats.binaryStl $ discreteAprox res obj

writeOBJ res = writeObject res NormedTriangleMeshFormats.obj

writeTHREEJS res = writeObject res TriangleMeshFormats.jsTHREE

writeGCodeHacklabLaser res = writeObject res PolylineFormats.hacklabLaserGCode

writeSCAD3 :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeSCAD3 res filename obj = LT.writeFile filename $ SymbolicFormats.scad3 res obj

writeSCAD2 :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writeSCAD2 res filename obj = LT.writeFile filename $ SymbolicFormats.scad2 res obj

writePNG res = writeObject' res ImageFormatCodecs.savePngImage
