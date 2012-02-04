-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Default where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Data.Map (Map, fromList)

defaultObjects :: VariableLookup -- = Map String OpenscadObj
defaultObjects = fromList $ 
	defaultConstants
	++ defaultFunctions
	++ defaultFunctions2
	++ defaultFunctionsSpecial

defaultConstants = map (\(a,b) -> (a, ONum b))
	[("pi", pi)]

defaultFunctions = map (\(a,b) -> (a, numericOFunc b))
	[
		("sin",   sin),
		("cos",   cos),
		("tan",   tan),
		("abs",   abs),
		("sign",  signum),
		("floor", fromIntegral . floor ),
		("ceil",  fromIntegral . ceiling ),
		("exp",   exp)
	]

defaultFunctions2 = map (\(a,b) -> (a, numericOFunc2 b))
	[
		("max", max),
		("min", min)
	]

defaultFunctionsSpecial = [("map", mapfunc)]

-- Stupid functions for convering to openscad objects follow:

mapfunc = OFunc $ \oObj -> case oObj of
	OFunc f -> OFunc $ \oObj2 -> case oObj2 of
		OList l -> OList $ map f l
		_ -> OUndefined
	_ -> OUndefined

numericOFunc f = OFunc $ \oObj -> case oObj of
	ONum n -> ONum $ f n
	_ -> OUndefined


numericOFunc2 f = OFunc $ \oObj -> case oObj of
	ONum n -> OFunc $ \oObj2 -> case oObj2 of
		ONum n2 -> ONum $ f n n2
		_ -> OUndefined
	_ -> OUndefined


