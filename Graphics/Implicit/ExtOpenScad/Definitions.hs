-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Graphics.Implicit.ExtOpenScad.Definitions where

import Graphics.Implicit.Definitions
import Data.Typeable (TypeRep)
import Data.Map (Map)

-- Lets make it easy to change the object types we're using :)

-- | The 2D object type to be used in ExtOpenScad
type Obj2Type = SymbolicObj2
-- | The 3D object type to be used in ExtOpenScad
type Obj3Type = SymbolicObj3

-- | To look up OpenscadObj variables with a string name
type VariableLookup = Map String OpenscadObj

-- | Objects for our OpenSCAD-like language
data OpenscadObj = OUndefined 
		 | OBool Bool 
		 | ONum ℝ
		 | OList [OpenscadObj]
		 | OString String
		 | OFunc ( OpenscadObj -> OpenscadObj ) 
		 | OModule (ArgParser ComputationStateModifier)
		 | OError [String]

-- | We'd like to be able to turn OpenscadObjs into a given Haskell type
class OTypeMirror a where
	fromOObj :: OpenscadObj -> Maybe a
	toOObj :: a -> OpenscadObj

instance OTypeMirror OpenscadObj where
	fromOObj a = Just a
	toOObj a = a

instance OTypeMirror ℝ where
	fromOObj (ONum n) = Just n
	fromOObj _ = Nothing
	toOObj n = ONum n

instance OTypeMirror ℕ where
	fromOObj (ONum n) = if n == fromIntegral (floor n) then Just (floor n) else Nothing
	fromOObj _ = Nothing
	toOObj n = ONum $ fromIntegral n

instance OTypeMirror Bool where
	fromOObj (OBool b) = Just b
	fromOObj _ = Nothing
	toOObj b = OBool b

instance OTypeMirror String where
	fromOObj (OString str) = Just str
	fromOObj _ = Nothing
	toOObj str = OString str

instance forall a. (Eq a, OTypeMirror a) => OTypeMirror [a] where
	fromOObj (OList list) = 
		let 
			maybeAList = map (\obj -> fromOObj obj :: Maybe a) list
		in if all (/=Nothing) maybeAList
		then Just $ map (\(Just aObj) -> aObj) maybeAList
		else Nothing
	fromOObj _ = Nothing
	toOObj list = OList $ map toOObj list

instance forall a b. (Eq a, OTypeMirror a, Eq b, OTypeMirror b) => OTypeMirror (a,b) where
	fromOObj (OList (x:y:[])) = 
		case (fromOObj x :: Maybe a, fromOObj y :: Maybe b) of
			(Just a, Just b) -> Just (a,b)
			_  -> Nothing
	fromOObj _ = Nothing
	toOObj (a,b) = OList [toOObj a, toOObj b]


instance forall a b c. (Eq a, OTypeMirror a, Eq b, OTypeMirror b, Eq c, OTypeMirror c) => 
				OTypeMirror (a,b,c) where
	fromOObj (OList (x:y:z:[])) = 
		case (fromOObj x :: Maybe a, fromOObj y :: Maybe b, fromOObj z :: Maybe c) of
			(Just a, Just b, Just c) -> Just (a,b,c)
			_  -> Nothing
	fromOObj _ = Nothing
	toOObj (a,b,c) = OList [toOObj a, toOObj b, toOObj c]


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

-- | Handles parsing arguments to modules
data ArgParser a 
                 -- | For actual argument entries:
                 --   ArgParser (argument name) (default) (doc) (next Argparser...)
                 = ArgParser String (Maybe OpenscadObj) String (OpenscadObj -> ArgParser a) 
                 -- | For returns:
                 --   ArgParserTerminator (return value)
                 | ArgParserTerminator a 
                 -- | For failure:
                 --   ArgParserFail (error message)
                 | ArgParserFail String
                 -- | For internal use only:
                 --   ArgParserAccumulator [error message] [documentation: name, default, description]
                 --      child
                 | ArgParserAccumulator [String] [(String, Maybe String, String)] (ArgParser a)

type ComputationState = IO (VariableLookup, [Obj2Type], [Obj3Type])

type ComputationStateModifier = ComputationState -> ComputationState

coerceNum (ONum n) = n
coerceNum _ = sqrt (-1)

coerceBool (OBool b) = b
coerceBool _ = False
