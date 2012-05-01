-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- Let's make it convenient to run our extended openscad format code

-- Let's be explicit about what we're getting from where :)
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Graphics.Implicit (runOpenscad, writeSVG, writeSTL, writeOBJ, writeGCodeHacklabLaser)
import Graphics.Implicit.ExtOpenScad.Definitions (OpenscadObj (ONum))
import Data.Map as Map

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

-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExport :: String -> String -> IO ()
executeAndExport content targetname = case runOpenscad content of
	Left err -> putStrLn $ show $ err
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let
			res = case Map.lookup "$res" vars of 
				Nothing -> 1
				Just (ONum n) -> n
				Just (_) -> 1
		case s of 
			(_, [], [])   -> putStrLn "Nothing to render"
			(_, x:xs, []) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname ++ ".svg"
				putStrLn $ show x
				writeSVG res (targetname ++ ".svg") x
			(_, _, x:xs)  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname++ ".stl"
				putStrLn $ show x
				writeSTL res (targetname ++ ".stl") x

-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExportSpecifiedTargetType :: String -> String -> String -> IO ()
executeAndExportSpecifiedTargetType content targetname formatname = case runOpenscad content of
	Left err -> putStrLn $ show $ err
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let
			res = case Map.lookup "$res" vars of 
				Nothing -> 1
				Just (ONum n) -> n
				Just (_) -> 1
		case (formatname, s) of 
			(_, (_, [], []))   -> putStrLn "Nothing to render"
			("svg", (_, x:xs, _)) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname
				writeSVG res targetname x
			("ngc", (_, x:xs, _)) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname
				writeGCodeHacklabLaser res targetname x
			("stl", (_, _, x:xs))  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname
				writeSTL res targetname x
			("obj", (_, _, x:xs))  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname
				writeOBJ res targetname x
			(otherFormat, _) -> putStrLn $ "Unrecognized format: " ++ otherFormat

		

main :: IO()
main = do
	args <- getArgs
	case length args of
		0 -> putStrLn $ 
			"syntax: extopenscad inputfile.escad [outputfile.format]\n"
			++ "eg. extopenscad input.escad out.stl"
		1 -> do
			f <- openFile (args !! 0) ReadMode
			content <- hGetContents f 
			executeAndExport content (strip $ args !! 0)
			hClose f
		2 -> do
			f <- openFile (args !! 0) ReadMode
			content <- hGetContents f 
			executeAndExportSpecifiedTargetType content (args !! 1) (fileType $ args !! 1)
			hClose f
