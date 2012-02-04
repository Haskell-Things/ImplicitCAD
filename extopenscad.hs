-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- Let's make it convenient to run our extended openscad format code

-- Let's be explicit about what we're getting from where :)
import System (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Graphics.Implicit (runOpenscad, writeSVG, writeSTL)
import Graphics.Implicit.ExtOpenScad.Definitions (OpenscadObj (ONum))
import Data.Map as Map

-- | strip a .scad or .escad file to its basename.
strip :: String -> String
strip filename = case reverse filename of
	'd':'a':'c':'s':'.':xs     -> reverse xs
	'd':'a':'c':'s':'e':'.':xs -> reverse xs
	_                          -> filename

-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExport :: String -> String -> IO ()
executeAndExport content targetname = case runOpenscad content of
	Left err -> putStrLn $ show $ err
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let {
			res = case Map.lookup "$res" vars of 
				Nothing -> 1
				Just (ONum n) -> n
				Just (_) -> 1
		} in case s of 
			(_, [], [])   -> putStrLn "Nothing to render"
			(_, x:xs, []) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname ++ ".svg"
				writeSVG res (targetname ++ ".svg") x
			(_, _, x:xs)  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname++ ".stl"
				writeSTL res (targetname ++ ".stl") x
		

main :: IO()
main = do
	args <- getArgs
	case length args of
		0 -> putStrLn "syntax: extopenscad file.escad"
		_ -> do
			f <- openFile (args !! 0) ReadMode
			content <- hGetContents f 
			executeAndExport content (strip $ args !! 0)
			hClose f

