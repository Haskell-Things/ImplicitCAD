-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.OutputFormat
  ( OutputFormat (SVG, SCAD, PNG, GCode, ASCIISTL, STL, THREEJS, OBJ, DXF),
    guessOutputFormat,
    formatExtensions,
    formatExtension,
    formats2D,
    formatIs2D,
    def2D,
    formats3D,
    formatIs3D,
    def3D,
  )
where

import Prelude (Bool, Eq, FilePath, Maybe, Read (readsPrec), Show(show), String, drop, error, flip, length, tail, take, ($), (<>), (==))
import Control.Applicative ((<$>))
-- For making the format guesser case insensitive when looking at file extensions.
import Data.Char (toLower)
import Data.Default.Class (Default(def))
import Data.List (lookup, elem)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
-- For handling input/output files.
import System.FilePath (takeExtensions)

-- | A type serving to enumerate our output formats.
data OutputFormat
  = SVG
  | SCAD
  | PNG
  | GCode
  | ASCIISTL
  | STL
  | THREEJS
  | OBJ
  | DXF
--  | 3MF
  deriving (Show, Eq)

instance Default OutputFormat where
  def = STL

-- | Default 2D output format
def2D :: OutputFormat
def2D = SVG

-- | Default 3D output format
def3D :: OutputFormat
def3D = def

-- | All supported 2D formats
formats2D :: [OutputFormat]
formats2D = [GCode, DXF, PNG, SCAD, SVG]

-- | True for 2D capable `OutputFormat`s
formatIs2D :: OutputFormat -> Bool
formatIs2D  = flip elem formats2D

-- | All supported 3D formats
formats3D :: [OutputFormat]
formats3D = [ASCIISTL, OBJ, STL, SCAD, THREEJS]

-- | True for 3D capable `OutputFormat`s
formatIs3D :: OutputFormat -> Bool
formatIs3D = flip elem formats3D

-- | A list mapping file extensions to output formats.
formatExtensions :: [(String, OutputFormat)]
formatExtensions =
  [ ("svg", SVG),
    ("scad", SCAD),
    ("png", PNG),
    ("ngc", GCode),
    ("gcode", GCode),
    ("ascii.stl", ASCIISTL),
    ("asciistl", ASCIISTL),
    ("stl", STL),
    ("three.js", THREEJS),
    ("threejs", THREEJS),
    ("obj", OBJ),
    ("dxf", DXF)
--  ("3mf", 3MF)
  ]

-- | Lookup an output format for a given output file. Throw an error if one cannot be found.
guessOutputFormat :: FilePath -> OutputFormat
guessOutputFormat fileName =
  fromMaybe (error $ "Unrecognized output format: " <> ext) $
    readOutputFormat $ tail ext
  where
    ext = takeExtensions fileName

-- | Try to look up an output format from a supplied extension.
readOutputFormat :: String -> Maybe OutputFormat
readOutputFormat ext = lookup (toLower <$> ext) formatExtensions

-- | A Read instance for our output format. Used by 'auto' in our command line parser.
--   Reads a string, and evaluates to the appropriate OutputFormat.
instance Read OutputFormat where
  readsPrec _ myvalue =
    tryParse formatExtensions
    where
      tryParse :: [(String, OutputFormat)] -> [(OutputFormat, String)]
      tryParse [] = [] -- If there is nothing left to try, fail
      tryParse ((attempt, result) : xs) =
        if take (length attempt) myvalue == attempt
          then [(result, drop (length attempt) myvalue)]
          else tryParse xs

-- | Get filename extension for `OutputFormat`
formatExtension :: OutputFormat -> String
formatExtension fmt = fromMaybe
  (error $ "No extension defined for OutputFormat " <> show fmt)
  $ lookup fmt (swap <$> formatExtensions)
