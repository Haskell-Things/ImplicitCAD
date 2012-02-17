-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Definitions where

import Graphics.Implicit.Definitions
import Data.Map (Map)

-- Lets make it easy to change the object types we're using :)

-- | The 2D object type to be used in ExtOpenScad
type Obj2Type = SymbolicObj2
-- | The 3D object type to be used in ExtOpenScad
type Obj3Type = SymbolicObj3

type VariableLookup = Map String OpenscadObj

data OpenscadObj = OUndefined 
		 | OBool Bool 
		 | ONum ℝ
		 | OList [OpenscadObj]
		 | OString String
		 | OFunc ( OpenscadObj -> OpenscadObj ) 
		 | OModule (ArgParser ComputationStateModifier)
		 | OError [String]

objTypeStr (OUndefined) = "Undefined"
objTypeStr (OBool   _ ) = "Bool"
objTypeStr (ONum    _ ) = "Number"
objTypeStr (OList   _ ) = "List"
objTypeStr (OString _ ) = "String"
objTypeStr (OFunc   _ ) = "Function"
objTypeStr (OModule _ ) = "Module"
objTypeStr (OError  _ ) = "Error"

instance Show OpenscadObj where
	show OUndefined = "Undefined"
	show (OBool b) = show b
	show (ONum n) = show n
	show (OList l) = show l
	show (OString s) = show s
	show (OFunc f) = "<function>"
	show (OError msgs) = "Execution Error:\n" ++ foldl1 (\a b -> a ++ "\n" ++ b) msgs

data ArgParser a = ArgParser String (Maybe OpenscadObj) (OpenscadObj -> ArgParser a) 
                 | ArgParserTerminator a 
                 | ArgParserFail

type ComputationState = IO (VariableLookup, [Obj2Type], [Obj3Type])

type ComputationStateModifier = ComputationState -> ComputationState

coerceNum (ONum n) = n
coerceNum _ = sqrt (-1)

coerceBool (OBool b) = b
coerceBool _ = False
