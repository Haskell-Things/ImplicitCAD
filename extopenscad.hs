-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- Let's make it convenient to run our extended openscad format code

import System
import System.IO
import Graphics.Implicit
import Graphics.Implicit.ExtOpenScad.Definitions
import Data.Map as S

strip filename = case reverse filename of
	'd':'a':'c':'s':'.':xs     -> reverse xs
	'd':'a':'c':'s':'e':'.':xs -> reverse xs
	_                          -> filename

executeAndExport content targetname = case runOpenscad content of
	Left err -> putStrLn $ show $ err
	Right openscadProgram -> do 
		s@(vars, obj2s, obj3s) <- openscadProgram 
		let
			res = case S.lookup "$res" vars of 
				Nothing -> 1
				Just (ONum n) -> n
		in case s of
			(_, [], [])   -> putStrLn "Nothing to render"
			(_, x:xs, []) -> do
				putStrLn $ "Rendering 2D object to " ++ targetname ++ ".svg"
				writeSVG res (targetname ++ ".svg") x
			(_, _, x:xs)  -> do
				putStrLn $ "Rendering 3D object to " ++ targetname++ ".stl"
				writeSTL res (targetname ++ ".stl") x

		

main = do
	args <- getArgs
	case length args of
		0 -> putStrLn "syntax: extopenscad file.escad"
		_ -> do
			f <- openFile (args !! 0) ReadMode
			content <- hGetContents f 
			executeAndExport content (strip $ args !! 0)
			hClose f
