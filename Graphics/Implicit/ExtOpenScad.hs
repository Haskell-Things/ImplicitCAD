-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Default
import Graphics.Implicit.ExtOpenScad.Statements

import Prelude hiding (lookup)
import Data.Map hiding (map,foldl)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)


runComputationsDefault = runComputations $
	return (fromList funcs, [], [])


runOpenscad str = case parse (many1 computationStatement) ""  str of
	Right res -> Right $ runComputationsDefault res
	Left  err ->  Left err



funcs = [
		("sin", numericOFunc sin),
		("cos", numericOFunc cos),
		("tan", numericOFunc tan),
		("abs", numericOFunc abs),
		("sign", numericOFunc signum),
		("floor", numericOFunc (fromIntegral . floor) ),
		("ceil", numericOFunc (fromIntegral . ceiling) ),
		("exp", numericOFunc exp),
		("max", numericOFunc2 max),
		("min", numericOFunc2 min),
		("map", mapfunc)
	]

mapfunc = OFunc $ \oObj -> case oObj of
	OFunc f -> OFunc $ \oObj2 -> case oObj2 of
		OList l -> OList $ map f l
		_ -> OUndefined
	_ -> OUndefined
