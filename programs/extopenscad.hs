-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2014 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU GPL, see LICENSE

-- An interpreter to run extended OpenScad code. outputs STL, OBJ, SVG, SCAD, PNG, DXF, or GCODE.

-- Enable additional syntax to make our code more readable.
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

-- Let's be explicit about what we're getting from where :)

import Prelude (Read(readsPrec), Maybe(Just, Nothing), Either(Left, Right), IO, FilePath, Show, Eq, String, (++), ($), (*), (/), (==), (>), (**), (-), readFile, minimum, drop, error, map, fst, min, sqrt, tail, take, length, putStrLn, show, print, (>>=), lookup, Bool, id, return, unlines)

-- Our Extended OpenScad interpreter, and functions to write out files in designated formats.
import Graphics.Implicit (runOpenscad, writeSVG, writeDXF2, writeBinSTL, writeOBJ, writeSCAD2, writeSCAD3, writeGCodeHacklabLaser, writePNG2, writePNG3)

-- Functions for finding a box around an object, so we can define the area we need to raytrace inside of.
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)

-- Definitions of the datatypes used for 2D objects, 3D objects, and for defining the resolution to raytrace at.
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ)

-- Use default values when a Maybe is Nothing.
import Data.Maybe (fromMaybe, maybe)

-- For making the format guesser case insensitive when looking at file extensions.
import Data.Char (toLower)

import Data.List (intercalate)

-- To flip around formatExtensions. Used when looking up an extension based on a format.
import Data.Tuple (swap)

-- Functions and types for dealing with the types used by runOpenscad.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal (ONum), lookupVarIn, LanguageOpts(LanguageOpts), Message)

-- Operator to subtract two points. Used when defining the resolution of a 2d object.
import Data.AffineSpace ((.-.))

-- For defining the <> operator.
import Data.Monoid (Monoid, mappend)

import Control.Applicative ((<$>), (<*>))

-- NOTE: make sure we don't import (<>) in new versions.
import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch)

-- For handling input/output files.
import System.FilePath (splitExtension)

import System.IO

-- | The following is needed to ensure backwards/forwards compatibility
-- | with old versions of Data.Monoid:
infixr 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- | Our command line options.
data ExtOpenScadOpts = ExtOpenScadOpts
    { outputFile :: Maybe FilePath
    , outputFormat :: Maybe OutputFormat
    , resolution :: Maybe ℝ
    , inputFile :: FilePath
    , alternateParser :: Bool
    , openScadCompatibility :: Bool
    , messageOutputFile :: Maybe FilePath
    }

-- | A type serving to enumerate our output formats.
data OutputFormat
    = SVG
    | SCAD
    | PNG
    | GCode
    | STL
    | OBJ
--  | 3MF
    | DXF
    deriving (Show, Eq)

-- | A list mapping file extensions to output formats.
formatExtensions :: [(String, OutputFormat)]
formatExtensions =
    [ ("svg", SVG)
    , ("scad", SCAD)
    , ("png", PNG)
    , ("ngc", GCode)
    , ("gcode", GCode)
    , ("stl", STL)
    , ("obj", OBJ)
--  , ("3mf", 3MF)
    , ("dxf", DXF)
    ]

-- | Lookup an output format for a given output file. Throw an error if one cannot be found.
guessOutputFormat :: FilePath -> OutputFormat
guessOutputFormat fileName =
    fromMaybe (error $ "Unrecognized output format: " <> ext)
    $ readOutputFormat $ tail ext
    where
        (_,ext) = splitExtension fileName

-- | The parser for our command line arguments.
extOpenScadOpts :: Parser ExtOpenScadOpts
extOpenScadOpts = ExtOpenScadOpts
    <$> optional (
      strOption
        (  short 'o'
        <> long "output"
        <> metavar "FILE"
        <> help "Output file name"
        )
      )
    <*> optional (
      option auto
        (  short 'f'
        <> long "format"
        <> metavar "FORMAT"
        <> help "Output format"
        )
      )
    <*> optional (
      option auto
        (  short 'r'
        <> long "resolution"
        <> metavar "RES"
        <> help "Approximation quality (smaller is better)"
        )
      )
    <*> argument str
        (  metavar "FILE"
        <> help "Input extended OpenSCAD file"
        )
    <*> switch
        (  short 'A'
        <> long "alternate-parser"
        <> help "Use the experimental alternate parser which will work with fewer ExtOpenScad files and more OpenSCAD files"
        )
    <*> switch
        (  short 'O'
        <> long "openscad-compatibility"
        <> help "Favour compatibility with OpenSCAD semantics, where they are incompatible with ExtOpenScad semantics"
        )
    <*> optional (
      strOption
        (  short 'e'
        <> long "echo-output"
        <> metavar "FILE"
        <> help "Output file name for echo statements"
        )
      )


-- | Try to look up an output format from a supplied extension.
readOutputFormat :: String -> Maybe OutputFormat
readOutputFormat ext = lookup (map toLower ext) formatExtensions

-- | A Read instance for our output format. Used by 'auto' in our command line parser.
--   Reads a string, and evaluates to the appropriate OutputFormat.
instance Read OutputFormat where
    readsPrec _ myvalue =
        tryParse formatExtensions
        where
          tryParse :: [(String, OutputFormat)] -> [(OutputFormat, String)]
          tryParse [] = []    -- If there is nothing left to try, fail
          tryParse ((attempt, result):xs) =
              if take (length attempt) myvalue == attempt
              then [(result, drop (length attempt) myvalue)]
              else tryParse xs

-- | Find the resolution to raytrace at.
getRes :: (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message]) -> ℝ
-- | First, use a resolution specified by a variable in the input file.
getRes (lookupVarIn "$res" -> Just (ONum res), _, _, _) = res
-- | Use a resolution chosen for 3D objects.
-- FIXME: magic numbers.
getRes (vars, _, obj:_, _) =
    let
        ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
        (x,y,z) = (x2-x1, y2-y1, z2-z1)
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z)**(1/3) / 22)
-- | Use a resolution chosen for 2D objects.
-- FIXME: magic numbers.
getRes (vars, obj:_, _, _) =
    let
        (p1,p2) = getBox2 obj
        (x,y) = p2 .-. p1
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0 -> min (min x y/2) (sqrt(x*y/qual) / 30)
        _                    -> min (min x y/2) (sqrt(x*y) / 30)
-- | fallthrough value.
getRes _ = 1

-- | Output a file containing a 3D object.
export3 :: Maybe OutputFormat -> ℝ -> FilePath -> SymbolicObj3 -> IO ()
export3 posFmt res output obj =
    case posFmt of
        Just STL  -> writeBinSTL res output obj
        Just SCAD -> writeSCAD3 res output obj
        Just OBJ  -> writeOBJ res output obj
        Just PNG  -> writePNG3 res output obj
        Nothing   -> writeBinSTL res output obj
        Just fmt  -> putStrLn $ "Unrecognized 3D format: "<>show fmt

-- | Output a file containing a 2D object.
export2 :: Maybe OutputFormat -> ℝ -> FilePath -> SymbolicObj2 -> IO ()
export2 posFmt res output obj =
    case posFmt of
        Just SVG   -> writeSVG res output obj
        Just DXF   -> writeDXF2 res output obj
        Just SCAD  -> writeSCAD2 res output obj
        Just PNG   -> writePNG2 res output obj
        Just GCode -> writeGCodeHacklabLaser res output obj
        Nothing    -> writeSVG res output obj
        Just fmt   -> putStrLn $ "Unrecognized 2D format: "<>show fmt

messageOutputHandle :: ExtOpenScadOpts -> IO Handle
messageOutputHandle args = maybe (return stdout) (`openFile` WriteMode) (messageOutputFile args)

-- | Interpret arguments, and render the object defined in the supplied input file.
run :: ExtOpenScadOpts -> IO ()
run args = do

    putStrLn "Loading File."
    content <- readFile (inputFile args)

    let format =
            case () of
                _ | Just fmt <- outputFormat args -> Just fmt
                _ | Just file <- outputFile args  -> Just $ guessOutputFormat file
                _                                 -> Nothing
        languageOpts = LanguageOpts (alternateParser args) (openScadCompatibility args)
        openscadProgram = runOpenscad languageOpts content
    putStrLn "Processing File."

    hMessageOutput <- messageOutputHandle args

    s@(_, obj2s, obj3s, messages) <- openscadProgram
    let res = maybe (getRes s) id (resolution args)
    let basename = fst (splitExtension $ inputFile args)
    let posDefExt = case format of
                        Just f  -> Prelude.lookup f (map swap formatExtensions)
                        Nothing -> Nothing -- We don't know the format -- it will be 2D/3D default
    hPutStr hMessageOutput $ unlines $ map show messages
    case (obj2s, obj3s) of
        ([], [obj]) -> do
            let output = fromMaybe
                             (basename ++ "." ++ fromMaybe "stl" posDefExt)
                             (outputFile args)
            putStrLn $ "Rendering 3D object to " ++ output
            putStrLn $ "With resolution " ++ show res
            putStrLn $ "In box " ++ show (getBox3 obj)
            putStrLn $ show obj
            export3 format res output obj
        ([obj], []) -> do
            let output = fromMaybe
                             (basename ++ "." ++ fromMaybe "svg" posDefExt)
                             (outputFile args)
            putStrLn $ "Rendering 2D object to " ++ output
            putStrLn $ "With resolution " ++ show res
            putStrLn $ "In box " ++ show (getBox2 obj)
            putStrLn $ show obj
            export2 format res output obj
        ([], []) -> putStrLn "No objects to render."
        _        -> putStrLn "A mixture of 2D and 3D objects, what do you want to render?"

-- | The entry point. Use the option parser then run the extended OpenScad code.
main :: IO ()
main = execParser opts >>= run
    where
        opts= info (helper <*> extOpenScadOpts)
              ( fullDesc
              <> progDesc "ImplicitCAD: Extended OpenSCAD interpreter."
              <> header "extopenscad - Extended OpenSCAD"
              )
