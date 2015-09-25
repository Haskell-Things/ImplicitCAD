-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE ViewPatterns, PatternGuards #-}

-- Let's make it convenient to run our extended openscad format code

-- Let's be explicit about what we're getting from where :)
import Graphics.Implicit (runOpenscad, writeSVG, writeBinSTL, writeOBJ, writeSCAD3, writeSCAD2, writeGCodeHacklabLaser, writePNG2, writePNG3)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.Definitions (xmlErrorOn, SymbolicObj2, SymbolicObj3)
import qualified Data.Map as Map hiding (null)
import Data.Maybe as Maybe
import Data.Char
import Data.Tuple (swap)
import Data.IORef (writeIORef)
import Data.AffineSpace
import Control.Applicative
-- The following is needed to ensure backwards/forwards compatibility
-- make sure we don't import (<>) in new versions.
import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, switch, value, long, short, option, metavar, execParser, Parser)
import System.FilePath

import Data.Monoid ((<>), mappend, mempty)

data ExtOpenScadOpts = ExtOpenScadOpts
    { outputFile :: Maybe FilePath
    , outputFormat :: Maybe OutputFormat
    , resolution :: Maybe Float
    , xmlError :: Bool
    , inputFile :: FilePath
    }

data OutputFormat
    = SVG
    | SCAD
    | PNG
    | GCode
    | STL
    | OBJ
    deriving (Show, Eq, Ord)

formatExtensions :: [(String, OutputFormat)]
formatExtensions =
    [ ("svg", SVG)
    , ("scad", SCAD)
    , ("png", PNG)
    , ("ngc", GCode)
    , ("stl", STL)
    , ("obj", OBJ)
    ]

readOutputFormat :: Monad m => String -> m OutputFormat
readOutputFormat ext = case lookup (map toLower ext) formatExtensions of
    Nothing -> fail ("unknown extension: "++ext)
    Just x -> return x

guessOutputFormat :: FilePath -> OutputFormat
guessOutputFormat fileName =
    Maybe.fromMaybe (error $ "Unrecognized output format: "<>ext)
    $ readOutputFormat $ tail ext
    where
        (_,ext) = splitExtension fileName

extOpenScadOpts :: Parser ExtOpenScadOpts
extOpenScadOpts =
    ExtOpenScadOpts
    <$> option (pure <$> str)
        (  short 'o'
        <> long "output"
        <> value Nothing
        <> metavar "FILE"
        <> help "Output file name"
        )
    <*> option (pure <$> (readOutputFormat =<< str))
        (  short 'f'
        <> long "format"
        <> value Nothing
        <> metavar "FORMAT"
        <> help "Output format"
        )
    <*> option (pure <$> auto)
        (  short 'r'
        <> long "resolution"
        <> value Nothing
        <> metavar "RES"
        <> help "Approximation quality"
        )
    <*> switch
        (  long "xml-error"
        <> help "Report XML errors"
        )
    <*> argument str ( metavar "FILE" )

getRes (Map.lookup "$res" -> Just (ONum res), _, _) = res

getRes (varlookup, _, obj:_) =
    let
        ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
        (x,y,z) = (x2-x1, y2-y1, z2-z1)
    in case Maybe.fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z     )**(1/3) / 22)

getRes (varlookup, obj:_, _) =
    let
        (p1,p2) = getBox2 obj
        (x,y) = p2 .-. p1
    in case Maybe.fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0 -> min (min x y/2) (sqrt (x*y/qual) / 30)
        _                    -> min (min x y/2) (sqrt (x*y     ) / 30)

getRes _ = 1

export3 :: Maybe OutputFormat -> Float -> FilePath -> SymbolicObj3 -> IO ()
export3 posFmt res output obj =
    case posFmt of
        Just STL  -> writeBinSTL res output obj
        Just SCAD -> writeSCAD3 res output obj
        Just OBJ  -> writeOBJ res output obj
        Just PNG  -> writePNG3 res output obj
        Nothing   -> writeBinSTL res output obj
        Just fmt  -> putStrLn $ "Unrecognized 3D format: "<>show fmt

export2 :: Maybe OutputFormat -> Float -> FilePath -> SymbolicObj2 -> IO ()
export2 posFmt res output obj =
    case posFmt of
        Just SVG   -> writeSVG res output obj
        Just SCAD  -> writeSCAD2 res output obj
        Just PNG   -> writePNG2 res output obj
        Just GCode -> writeGCodeHacklabLaser res output obj
        Nothing    -> writeSVG res output obj
        Just fmt   -> putStrLn $ "Unrecognized 2D format: "<>show fmt

main :: IO()
main = do
    args <- execParser
        $ info (helper <*> extOpenScadOpts)
          ( fullDesc
          <> progDesc "Extended OpenSCAD"
          <> header "extopenscad - Extended OpenSCAD"
          )
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
            s@(vars, obj2s, obj3s) <- openscadProgram
            let res = maybe (getRes s) id (resolution args)
            let basename = fst (splitExtension $ inputFile args)
            let posDefExt = case format of
                                Just f  -> lookup f (map swap formatExtensions)
                                Nothing -> Nothing -- We don't know the format -- it will be 2D/3D default
                                {-let Just defExtension = lookup format (map swap formatExtensions)
                                in maybe (fst (splitExtension $ inputFile args)<>"."<>defExtension) id
                                $ outputFile args-}
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
                                     (basename ++ "." ++ fromMaybe "stl" posDefExt)
                                     (outputFile args)
                    putStrLn $ "Rendering 2D object to " ++ output
                    putStrLn $ "With resolution " ++ show res
                    putStrLn $ "In box " ++ show (getBox2 obj)
                    export2 format res output obj
                ([], []) -> putStrLn "No objects to render"
                _        -> putStrLn "Multiple objects, what do you want to render?"

