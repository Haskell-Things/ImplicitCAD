-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE ViewPatterns #-}

-- Let's make it convenient to run our extended openscad format code

-- Let's be explicit about what we're getting from where :)
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Graphics.Implicit (runOpenscad, writeSVG, writeBinSTL, writeOBJ, writeSCAD3, writeSCAD2, writeGCodeHacklabLaser, writeTHREEJS, writePNG2, writePNG3)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.Definitions (xmlErrorOn, errorMessage)
import Data.Map as Map hiding (null)
import Data.Maybe as Maybe
import Text.ParserCombinators.Parsec (errorPos, sourceLine)
import Text.ParserCombinators.Parsec.Error
import Data.IORef (writeIORef)
import Data.VectorSpace

-- | strip a .scad or .escad file to its basename.
strip :: String -> String
strip filename = case reverse filename of
	'd':'a':'c':'s':'.':xs     -> reverse xs
	'd':'a':'c':'s':'e':'.':xs -> reverse xs
	_                          -> filename

-- | Get the file type ending of a file
--  eg. "foo.stl" -> "stl"
fileType filename = reverse $ beforeFirstPeriod $ reverse filename
	where
		beforeFirstPeriod []       = [] 
		beforeFirstPeriod ('.':xs) = []
		beforeFirstPeriod (  x:xs) = x : beforeFirstPeriod xs

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
		(x,y) = p2 ^-^ p1
	in case Maybe.fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
		ONum qual | qual > 0 -> min (min x y/2) ((x*y/qual)**0.5 / 30)
		_                    -> min (min x y/2) ((x*y     )**0.5 / 30)

getRes _ = 1

-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExport :: String -> String -> IO ()
executeAndExport content targetname = case runOpenscad content of
	Left err -> 
		let
			line = sourceLine . errorPos $ err
			msgs = errorMessages err
		in errorMessage line $ showErrorMessages 
			"or" "unknown parse error" "expecting" "unexpected" "end of input"
            (errorMessages err)
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let
			res = getRes s
		case s of 
			(_, [], [])   -> putStrLn "Nothing to render"
			(_, x:xs, []) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname ++ ".svg"
				putStrLn $ show x
				writeSVG res (targetname ++ ".svg") x
			(_, _, x:xs)  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname++ ".stl"
				putStrLn $ show x
				writeBinSTL res (targetname ++ ".stl") x

-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExportSpecifiedTargetType :: String -> String -> String -> IO ()
executeAndExportSpecifiedTargetType content targetname formatname = case runOpenscad content of
	Left err -> putStrLn $ show $ err
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let
			res = getRes s
		if not (null obj2s)
		then do
			let obj = head obj2s
			putStrLn $ "Rendering 2D object to " ++ targetname
			putStrLn $ "With resolution " ++ show res
			putStrLn $ "In box " ++ show (getBox2 obj)
			putStrLn $ show obj
			case formatname of
				"svg" -> writeSVG   res targetname obj
				"scad"-> writeSCAD2 res targetname obj
				"png" -> writePNG2  res targetname obj
				"ngc" -> writeGCodeHacklabLaser res targetname obj
				_     -> putStrLn $ "Unrecognized 2D format: " ++ formatname
		else if not (null obj3s)
		then do
			let obj = head obj3s
			putStrLn $ "Rendering 3D object to " ++ targetname
			putStrLn $ "With resolution " ++ show res
			putStrLn $ "In box " ++ show (getBox3 obj)
			putStrLn $ show obj
			case formatname of
				"stl" -> writeBinSTL res targetname obj
				"scad"-> writeSCAD3  res targetname obj
				"obj" -> writeOBJ    res targetname obj
				"png" -> writePNG3   res targetname obj
				_     -> putStrLn $ "Unrecognized 3D format: " ++ formatname
		else
			putStrLn "Nothing to render."
		

main :: IO()
main = do
	args <- getArgs
	if Prelude.null args || args == ["--help"] || args == ["-help"]
		then putStrLn $ 
			"syntax: extopenscad inputfile.escad [outputfile.format]\n"
			++ "eg. extopenscad input.escad out.stl"
		else do
			let
				args' = if head args == "-xml-error" then tail args else args
			writeIORef xmlErrorOn (head args == "-xml-error")
			case length args' of
				1 -> do
					f <- openFile (args' !! 0) ReadMode
					content <- hGetContents f 
					executeAndExport content (strip $ args' !! 0)
					hClose f
				2 -> do
					f <- openFile (args' !! 0) ReadMode
					content <- hGetContents f 
					executeAndExportSpecifiedTargetType 
						content (args' !! 1) (fileType $ args' !! 1)
					hClose f
				_ -> putStrLn $ 
					"syntax: extopenscad inputfile.escad [outputfile.format]\n"
					++ "eg. extopenscad input.escad out.stl"


