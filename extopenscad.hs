-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE ViewPatterns, PatternGuards #-}

-- Let's make it convenient to run our extended openscad format code

-- Let's be explicit about what we're getting from where :)
import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Graphics.Implicit (runOpenscad, writeSVG, writeBinSTL, writeOBJ, writeSCAD3, writeSCAD2, writeGCodeHacklabLaser, writeTHREEJS, writePNG2, writePNG3)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.Definitions (xmlErrorOn, errorMessage, SymbolicObj2, SymbolicObj3)
import qualified Data.Map as Map hiding (null)
import Data.Maybe as Maybe
import Data.Monoid (Monoid, mappend)
import Data.Tuple (swap)
import Text.ParserCombinators.Parsec (errorPos, sourceLine)
import Text.ParserCombinators.Parsec.Error
import Data.IORef (writeIORef)
import Data.AffineSpace
import Options.Applicative
import System.FilePath

-- Backwards compatibility with old versions of Data.Monoid:
infixr 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

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

readOutputFormat :: String -> Maybe OutputFormat
readOutputFormat ext = lookup ext formatExtensions

guessOutputFormat :: FilePath -> OutputFormat
guessOutputFormat fileName =
	maybe (error $ "Unrecognized output format: "<>ext) id
	$ readOutputFormat $ tail ext
	where
		(_,ext) = splitExtension fileName

extOpenScadOpts :: Parser ExtOpenScadOpts
extOpenScadOpts =
	ExtOpenScadOpts
	<$> nullOption
		(  short 'o'
		<> long "output"
		<> value Nothing
		<> metavar "FILE"
		<> reader (pure . str)
		<> help "Output file name"
		)
	<*> nullOption
		(  short 'f'
		<> long "format"
		<> value Nothing
		<> metavar "FILE"
		<> help "Output format"
		<> reader (pure . readOutputFormat)
		)
	<*> option
		(  short 'r'
		<> long "resolution"
		<> value Nothing
		<> metavar "RES"
		<> help "Approximation quality"
		)
	<*> switch
		( long "xml-error"
		& help "Report XML errors"
		)
	<*> argument str ( metavar "FILE" )

getRes (Map.lookup "$res" -> Just (ONum res), _, _) = res

getRes (varlookup, _, obj:_) =
	let
		((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
		(x,y,z) = (x2-x1, y2-y1, z2-z1)
	in case Maybe.fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
		ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
		_					  -> min (minimum [x,y,z]/2) ((x*y*z	 )**(1/3) / 22)

getRes (varlookup, obj:_, _) =
	let
		(p1,p2) = getBox2 obj
		(x,y) = p2 .-. p1
	in case Maybe.fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
		ONum qual | qual > 0 -> min (min x y/2) ((x*y/qual)**0.5 / 30)
		_					 -> min (min x y/2) ((x*y	  )**0.5 / 30)

getRes _ = 1

export3 :: OutputFormat -> Float -> FilePath -> SymbolicObj3 -> IO ()
export3 fmt res output obj =
	case fmt of
		STL	  -> writeBinSTL res output obj
		SCAD  -> writeSCAD3 res output obj
		OBJ	  -> writeOBJ res output obj
		PNG	  -> writePNG3 res output obj
		_	   -> putStrLn $ "Unrecognized 3D format: "<>show fmt

export2 :: OutputFormat -> Float -> FilePath -> SymbolicObj2 -> IO ()
export2 fmt res output obj =
	case fmt of
		SVG	  -> writeSVG res output obj
		SCAD  -> writeSCAD2 res output obj
		PNG	  -> writePNG2 res output obj
		GCode -> writeGCodeHacklabLaser res output obj
		_	  -> putStrLn $ "Unrecognized 2D format: "<>show fmt

main :: IO()
main = do
	args <- execParser
		$ info (helper <*> extOpenScadOpts)
		  ( fullDesc
		  <> progDesc "Extended OpenSCAD"
		  <> header "extopenscad - Extended OpenSCAD"
		  )
	writeIORef xmlErrorOn (xmlError args)

	content <- readFile (inputFile args)
	let format = 
		case () of
			_ | Just fmt <- outputFormat args -> fmt
			_ | Just file <- outputFile args  -> guessOutputFormat file
	case runOpenscad content of
		Left err -> putStrLn $ show $ err
		Right openscadProgram -> do
			s@(vars, obj2s, obj3s) <- openscadProgram
			let res = maybe (getRes s) id (resolution args)
			let output =
				let Just defExtension = lookup format (map swap formatExtensions)
				in maybe (fst (splitExtension $ inputFile args)<>"."<>defExtension) id
				$ outputFile args
			case (obj2s, obj3s) of
				 ([], [obj]) -> do
					putStrLn $ "Rendering 3D object to " ++ output
					putStrLn $ "With resolution " ++ show res
					putStrLn $ "In box " ++ show (getBox3 obj)
					putStrLn $ show obj
					export3 format res output obj
				 ([obj], []) -> do
					putStrLn $ "Rendering 2D object to " ++ output
					putStrLn $ "With resolution " ++ show res
					putStrLn $ "In box " ++ show (getBox2 obj)
					putStrLn $ show obj
					export2 format res output obj
				 _ -> putStrLn "No objects to render"
