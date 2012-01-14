-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Definitions where

import Graphics.Implicit.Definitions
import Data.Map hiding (map,foldl)

type VariableLookup = Map String OpenscadObj

data OpenscadObj = OUndefined 
		 | OBool Bool 
		 | ONum ℝ
		 | OList [OpenscadObj]
		 | OString String
		 | OFunc ( OpenscadObj -> OpenscadObj ) 

instance Show OpenscadObj where
	show OUndefined = "Undefined"
	show (OBool b) = show b
	show (ONum n) = show n
	show (OList l) = show l
	show (OString s) = show s
	show (OFunc f) = "<function>"

type ComputationState = (VariableLookup, [Obj2], [Obj3], IO() )

type ComputationStateModifier = ComputationState -> ComputationState

coerceNum (ONum n) = n
coerceNum _ = sqrt (-1)

coerceBool (OBool b) = b
coerceBool _ = False
