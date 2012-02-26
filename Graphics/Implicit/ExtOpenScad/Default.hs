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

-- Missing standard ones:
-- rand, lookup, 

defaultConstants = map (\(a,b) -> (a, toOObj (b::ℝ) ))
	[("pi", pi)]

defaultFunctions = map (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ)))
	[
		("sin",   sin),
		("cos",   cos),
		("tan",   tan),
		("asin",  asin),
		("acos",  acos),
		("atan",  atan),
		("abs",   abs),
		("sign",  signum),
		("floor", fromIntegral . floor ),
		("ceil",  fromIntegral . ceiling ),
		("round", fromIntegral . round ),
		("exp",   exp),
		("ln",    log),
		("log",   log),
		("sign",  signum),
		("sqrt",  sqrt)
	]

defaultFunctions2 = map (\(a,b) -> (a, toOObj (b :: ℝ -> ℝ -> ℝ) ))
	[
		("max", max),
		("min", min),
		("atan2", atan2),
		("pow", (**))
	]

defaultFunctionsSpecial = 
	[
		("map", toOObj $ flip $ 
			(map :: (OpenscadObj -> OpenscadObj) -> [OpenscadObj] -> [OpenscadObj] ) 
		)
		
	]

