-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2014 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU GPL, see LICENSE

-- FIXME: add support for AMF.
-- An interpreter to run extended OpenScad code, outputing STL, OBJ, SVG, SCAD, PNG, or GCODE.

-- Enable additional syntax to make our code prettier.
{-# LANGUAGE ViewPatterns , PatternGuards #-}

-- Let's be explicit about what we're getting from where :)

-- Our Extended OpenScad interpreter, and functions to write out files in designated formats.
import Graphics.Implicit (runOpenscad, writeSVG, writeBinSTL, writeOBJ, writeSCAD2, writeSCAD3, writeGCodeHacklabLaser, writePNG2, writePNG3)

-- Functions for finding a box around an object, so we can define the area we need to raytrace inside of.
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)

-- Definitions of the datatypes used for 2D objects, 3D objects, and for defining the resolution to raytrace at.
-- Also, a function to turn on formatting errors in XML?.
import Graphics.Implicit.Definitions (xmlErrorOn, SymbolicObj2, SymbolicObj3, ℝ)

-- Use default values when a Maybe is Nothing.
import Data.Maybe (fromMaybe)

-- For making the format guesser case insensitive when looking at file extensions.
import Data.Char (toLower)

-- To flip around formatExtensions. Used when looking up an extension based on a format.
import Data.Tuple (swap)

-- FIXME: what is this XML stuff about?
import Data.IORef (writeIORef)

-- Functions and types for dealing with the types used by runOpenscad.
-- Note that Map is different than the maps used by the prelude functions.
import qualified Data.Map.Strict as Map (Map, lookup)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))

-- Operator to subtract two points. Used when defining the resolution of a 2d object.
import Data.AffineSpace ((.-.))

-- NOTE: make sure we don't import (<>) in new versions.
import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, switch,  long, short, option, metavar, execParser, Parser, optional, strOption)

-- For handling input/output files.
import System.FilePath (splitExtension)

-- The following is needed to ensure backwards/forwards compatibility
-- Backwards compatibility with old versions of Data.Monoid:
infixr 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- A datatype for containing our command line options.
data ExtOpenScadOpts = ExtOpenScadOpts
    { outputFile :: Maybe FilePath
    , outputFormat :: Maybe OutputFormat
    , resolution :: Maybe ℝ
    , xmlError :: Bool
    , inputFile :: FilePath
    }

-- A datatype enumerating our output file formats types.
data OutputFormat
    = SVG
    | SCAD
    | PNG
    | GCode
    | STL
    | OBJ
--  | AMF
    deriving (Show, Eq, Ord)

-- A list mapping file extensions to output formats.
formatExtensions :: [(String, OutputFormat)]
formatExtensions =
    [ ("svg", SVG)
    , ("scad", SCAD)
    , ("png", PNG)
    , ("ngc", GCode)
    , ("gcode", GCode)
    , ("stl", STL)
    , ("obj", OBJ)
--  , ("amf", AMF)
    ]

-- Lookup an output format for a given output file. Throw an error if one cannot be found.
guessOutputFormat :: FilePath -> OutputFormat
guessOutputFormat fileName =
    maybe (error $ "Unrecognized output format: "<>ext) id
    $ readOutputFormat $ tail ext
    where
        (_,ext) = splitExtension fileName

-- The parser for our command line arguments.
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
    <*> switch
        (  long "xml-error"
        <> help "Report XML errors"
        )
    <*> argument str
        (  metavar "FILE"
        <> help "Input extended OpenSCAD file"
        )

-- Try to look up an output format from a supplied extension.
readOutputFormat :: String -> Maybe OutputFormat
readOutputFormat ext = lookup (map toLower ext) formatExtensions

-- A Read instance for our output format. Used by 'auto' in our command line parser.
-- Reads a string, and evaluates to the appropriate OutputFormat.
instance Read OutputFormat where
    readsPrec _ myvalue =
        tryParse formatExtensions
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                  if (take (length attempt) myvalue) == attempt
                  then [(result, drop (length attempt) myvalue)]
                  else tryParse xs

-- Find the resolution to raytrace at.
getRes :: (Map.Map [Char] OVal, [SymbolicObj2], [SymbolicObj3]) -> ℝ
-- First, use a resolution specified by a variable in the input file.
getRes (Map.lookup "$res" -> Just (ONum res), _, _) = res
-- Use a resolution chosen for 3D objects.
-- FIXME: magic numbers.
getRes (varlookup, _, obj:_) =
    let
        ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
        (x,y,z) = (x2-x1, y2-y1, z2-z1)
    in case fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z)**(1/3) / 22)
-- Use a resolution chosen for 2D objects.
-- FIXME: magic numbers.
getRes (varlookup, obj:_, _) =
    let
        (p1,p2) = getBox2 obj
        (x,y) = p2 .-. p1
    in case fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0 -> min (min x y/2) ((x*y/qual)**0.5 / 30)
        _                    -> min (min x y/2) ((x*y)**0.5 / 30)
-- fallthrough value.
getRes _ = 1

-- Output a file containing a 3D object.
export3 :: Maybe OutputFormat -> ℝ -> FilePath -> SymbolicObj3 -> IO ()
export3 posFmt res output obj =
    case posFmt of
        Just STL  -> writeBinSTL res output obj
        Just SCAD -> writeSCAD3 res output obj
        Just OBJ  -> writeOBJ res output obj
        Just PNG  -> writePNG3 res output obj
        Nothing   -> writeBinSTL res output obj
        Just fmt  -> putStrLn $ "Unrecognized 3D format: "<>show fmt

-- Output a file containing a 2D object.
export2 :: Maybe OutputFormat -> ℝ -> FilePath -> SymbolicObj2 -> IO ()
export2 posFmt res output obj =
    case posFmt of
        Just SVG   -> writeSVG res output obj
        Just SCAD  -> writeSCAD2 res output obj
        Just PNG   -> writePNG2 res output obj
        Just GCode -> writeGCodeHacklabLaser res output obj
        Nothing    -> writeSVG res output obj
        Just fmt   -> putStrLn $ "Unrecognized 2D format: "<>show fmt

-- Interpret arguments, and render the object defined in the supplied input file.
run :: ExtOpenScadOpts -> IO()
run args = do
    writeIORef xmlErrorOn (xmlError args)

    putStrLn $ "Loading File."
    content <- readFile (inputFile args)

    let format =
            case () of
                _ | Just fmt <- outputFormat args -> Just $ fmt
                _ | Just file <- outputFile args  -> Just $ guessOutputFormat file
                _                                 -> Nothing
    putStrLn $ "Processing File."

    case runOpenscad content of
        Left err -> putStrLn $ show $ err
        Right openscadProgram -> do
            s@(_, obj2s, obj3s) <- openscadProgram
            let res = maybe (getRes s) id (resolution args)
            let basename = fst (splitExtension $ inputFile args)
            let posDefExt = case format of
                                Just f  -> Prelude.lookup f (map swap formatExtensions)
                                Nothing -> Nothing -- We don't know the format -- it will be 2D/3D default
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
                _        -> putStrLn "Multiple/No objects, what do you want to render?"

-- The entry point. Use the option parser then run the extended OpenScad code.
main :: IO()
main = execParser opts >>= run
    where
        opts= info (helper <*> extOpenScadOpts)
              ( fullDesc
              <> progDesc "ImplicitCAD: Extended OpenSCAD interpreter." 
              <> header "extopenscad - Extended OpenSCAD"
              )
