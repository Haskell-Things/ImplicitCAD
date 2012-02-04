-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Statements (computationStatement, runComputations)

import Text.ParserCombinators.Parsec (parse, many1)
import Control.Monad (liftM)

-- Small wrapper to handle parse errors, etc
runOpenscad str = case parse (many1 computationStatement) ""  str of
	Right res -> Right $ runComputationsDefault res
	Left  err ->  Left err

runComputationsDefault = runComputations $
	return (defaultObjects, [], [])


