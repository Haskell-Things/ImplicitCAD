{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.OutputFormat
  ( OutputFormat (SVG, SCAD, PNG, GCode, ASCIISTL, STL, OBJ, DXF),
    guessOutputFormat,
    formatExtensions,
  )
where

import Prelude (Eq, FilePath, Maybe, Read (readsPrec), Show, String, drop, error, length, lookup, tail, take, ($), (<>), (==))
import Control.Applicative ((<$>))
-- For making the format guesser case insensitive when looking at file extensions.
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
-- For handling input/output files.
import System.FilePath (splitExtension)

-- | A type serving to enumerate our output formats.
data OutputFormat
  = SVG
  | SCAD
  | PNG
  | GCode
  | ASCIISTL
  | STL
  | OBJ
  | DXF
--  | 3MF
  deriving (Show, Eq)

-- | A list mapping file extensions to output formats.
formatExtensions :: [(String, OutputFormat)]
formatExtensions =
  [ ("svg", SVG),
    ("scad", SCAD),
    ("png", PNG),
    ("ngc", GCode),
    ("gcode", GCode),
    ("stl", STL),
    ("asciistl", ASCIISTL),
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
    (_, ext) = splitExtension fileName

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
