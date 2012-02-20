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

defaultConstants = map (\(a,b) -> (a, toOObj (b::ℝ) ))
	[("pi", pi)]

defaultFunctions = map (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ)))
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

defaultFunctions2 = map (\(a,b) -> (a, toOObj (b :: ℝ -> ℝ -> ℝ) ))
	[
		("max", max),
		("min", min)
	]

defaultFunctionsSpecial = 
	[
		("map", toOObj $ flip $ 
			(map :: (OpenscadObj -> OpenscadObj) -> [OpenscadObj] -> [OpenscadObj] ) 
		)
		
	]

