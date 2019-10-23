-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Implicit.ExtOpenScad.Util.OVal(OTypeMirror, (<||>), fromOObj, toOObj, divideObjs, caseOType, oTypeStr, getErrors) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left,Right), Char, String, (==), fromInteger, floor, ($), (.), map, error, (++), show, head, flip, filter, not, return, head)

import Graphics.Implicit.Definitions(ℝ, ℕ, SymbolicObj2, SymbolicObj3, fromℕtoℝ)

import Graphics.Implicit.ExtOpenScad.Definitions (OVal(ONum, OBool, OString, OList, OFunc, OUndefined, OModule, OUModule, ONModule, OVargsModule, OError, OObj2, OObj3))

import Control.Monad (mapM, msum)

import Data.Maybe (fromMaybe, maybe)

-- for some minimal paralellism.
import Control.Parallel.Strategies(runEval, rpar, rseq)

-- Convert OVals (and Lists of OVals) into a given Haskell type
class OTypeMirror a where
    fromOObj :: OVal -> Maybe a
    fromOObjList :: OVal -> Maybe [a]
    fromOObjList (OList list) = mapM fromOObj list
    fromOObjList _ = Nothing
    {-# INLINABLE fromOObjList #-}
    toOObj :: a -> OVal

instance OTypeMirror OVal where
    fromOObj = Just
    {-# INLINABLE fromOObj #-}
    toOObj a = a

instance OTypeMirror ℝ where
    fromOObj (ONum n) = Just n
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj = ONum

instance OTypeMirror ℕ where
    fromOObj (ONum n) = if n == fromInteger (floor n) then Just (floor n) else Nothing
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj = ONum . fromℕtoℝ

instance OTypeMirror Bool where
    fromOObj (OBool b) = Just b
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj = OBool

-- We don't actually use single chars, this is to compile lists of chars (AKA strings) after passing through OTypeMirror [a]'s fromOObj.
-- This lets us handle strings without overlapping the [a] case.
instance OTypeMirror Char where
    fromOObj (OString str) = Just $ head str
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    fromOObjList (OString str) = Just str
    fromOObjList _ = Nothing
    toOObj a = OString [a]

instance (OTypeMirror a) => OTypeMirror [a] where
    fromOObj = fromOObjList
    {-# INLINABLE fromOObj #-}
    toOObj list = OList $ map toOObj list

instance (OTypeMirror a) => OTypeMirror (Maybe a) where
    fromOObj a = Just $ fromOObj a
    {-# INLINABLE fromOObj #-}
    toOObj (Just a) = toOObj a
    toOObj Nothing  = OUndefined

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (a,b) where
    fromOObj (OList [fromOObj -> Just a,fromOObj -> Just b]) = Just (a,b)
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj (a,b) = OList [toOObj a, toOObj b]

instance (OTypeMirror a, OTypeMirror b, OTypeMirror c) => OTypeMirror (a,b,c) where
    fromOObj (OList [fromOObj -> Just a,fromOObj -> Just b,fromOObj -> Just c]) =
        Just (a,b,c)
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj (a,b,c) = OList [toOObj a, toOObj b, toOObj c]

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (a -> b) where
    fromOObj (OFunc f) =  Just $ \input ->
        let
            oInput = toOObj input
            oOutput = f oInput
            output :: Maybe b
            output = fromOObj oOutput
        in
          fromMaybe (error $ "coercing OVal to a -> b isn't always safe; use a -> Maybe b"
                               ++ " (trace: " ++ show oInput ++ " -> " ++ show oOutput ++ " )") output
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}
    toOObj f = OFunc $ \oObj ->
        case fromOObj oObj :: Maybe a of
            Nothing  -> OError ["bad input type"]
            Just obj -> toOObj $ f obj

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (Either a b) where
    fromOObj (fromOObj -> Just (x :: a)) = Just $ Left  x
    fromOObj (fromOObj -> Just (x :: b)) = Just $ Right x
    fromOObj _ = Nothing
    {-# INLINABLE fromOObj #-}

    toOObj (Right x) = toOObj x
    toOObj (Left  x) = toOObj x

-- A string representing each type.
oTypeStr :: OVal -> String
oTypeStr OUndefined         = "Undefined"
oTypeStr (OBool          _ ) = "Bool"
oTypeStr (ONum           _ ) = "Number"
oTypeStr (OList          _ ) = "List"
oTypeStr (OString        _ ) = "String"
oTypeStr (OFunc          _ ) = "Function"
oTypeStr (OModule    _ _ _ ) = "Module"
oTypeStr (OUModule   _ _ _ ) = "User Defined Module"
oTypeStr (ONModule   _ _ _ ) = "Built-in Module"
oTypeStr (OVargsModule _ _ ) = "VargsModule"
oTypeStr (OError         _ ) = "Error"
oTypeStr (OObj2          _ ) = "2D Object"
oTypeStr (OObj3          _ ) = "3D Object"

getErrors :: OVal -> Maybe String
getErrors (OError er) = Just $ head er
getErrors (OList l)   = msum $ map getErrors l
getErrors _           = Nothing

caseOType :: forall c a. a -> (a -> c) -> c
caseOType = flip ($)

infixr 2 <||>
(<||>) :: forall desiredType out. (OTypeMirror desiredType)
    => (desiredType -> out)
    -> (OVal -> out)
    -> (OVal -> out)
(<||>) f g input =
    let
        coerceAttempt :: Maybe desiredType
        coerceAttempt = fromOObj input
    in
        maybe (g input) f coerceAttempt

-- separate 2d and 3d objects from a set of OVals.
divideObjs :: [OVal] -> ([SymbolicObj2], [SymbolicObj3], [OVal])
divideObjs children =
    runEval $ do
    obj2s <- rseq [ x | OObj2 x <- children ]
    obj3s <- rseq [ x | OObj3 x <- children ]
    objs <- rpar (filter (not . isOObj) children)
    return (obj2s, obj3s, objs)
      where
        isOObj  (OObj2 _) = True
        isOObj  (OObj3 _) = True
        isOObj  _         = False
