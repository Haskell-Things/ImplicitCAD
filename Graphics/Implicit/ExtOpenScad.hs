-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad (runOpenscad, OpenscadObj (..) ) where

import Graphics.Implicit.ExtOpenScad.Definitions (OpenscadObj (..) )
import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Statements (computationStatement)
import Graphics.Implicit.ExtOpenScad.Util.Computation (runComputations)

import Text.ParserCombinators.Parsec (parse, many1, many, space, eof)
import Control.Monad (liftM)

-- Small wrapper to handle parse errors, etc
runOpenscad str = case parse (do {s <- many1 computationStatement; many space; eof; return s}) ""  str of
	Right res -> Right $ runComputationsDefault res
	Left  err ->  Left err

runComputationsDefault = runComputations $
	return (defaultObjects, [], [])


