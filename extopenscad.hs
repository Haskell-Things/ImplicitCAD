-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- Let's make it convenient to run our extended openscad format code

import System
import System.IO
import Graphics.Implicit

main = do
	args <- getArgs
	case length args of
		0 -> putStrLn "syntax: extopenscad file.escad"
		_ -> do
			f <- openFile (args !! 0) ReadMode
			content <- hGetContents f 
			runOpenscad content
			hClose f
