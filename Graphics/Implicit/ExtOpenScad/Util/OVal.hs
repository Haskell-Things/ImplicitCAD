{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# LANGUAGE ViewPatterns, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Implicit.ExtOpenScad.Util.OVal where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import qualified Control.Monad as Monad
import Data.Maybe (isJust)

-- | We'd like to be able to turn OVals into a given Haskell type
class OTypeMirror a where
    fromOObj :: OVal -> Maybe a
    toOObj :: a -> OVal

instance OTypeMirror OVal where
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

instance {-# Overlapping #-} OTypeMirror String where
    fromOObj (OString str) = Just str
    fromOObj _ = Nothing
    toOObj str = OString str

instance forall a. (OTypeMirror a) => OTypeMirror (Maybe a) where
    fromOObj a = Just $ fromOObj a
    toOObj (Just a) = toOObj a
    toOObj Nothing  = OUndefined

instance {-# Overlappable #-} forall a. (OTypeMirror a) => OTypeMirror [a] where
    fromOObj (OList list) = Monad.sequence . map fromOObj $ list
    fromOObj _ = Nothing
    toOObj list = OList $ map toOObj list

instance forall a b. (OTypeMirror a, OTypeMirror b) => OTypeMirror (a,b) where
    fromOObj (OList ((fromOObj -> Just a):(fromOObj -> Just b):[])) = Just (a,b)
    fromOObj _ = Nothing
    toOObj (a,b) = OList [toOObj a, toOObj b]


instance forall a b c. (OTypeMirror a, OTypeMirror b, OTypeMirror c) => OTypeMirror (a,b,c) where
    fromOObj (OList ((fromOObj -> Just a):(fromOObj -> Just b):(fromOObj -> Just c):[])) = 
        Just (a,b,c)
    fromOObj _ = Nothing
    toOObj (a,b,c) = OList [toOObj a, toOObj b, toOObj c]

instance forall a b. (OTypeMirror a, OTypeMirror b) => OTypeMirror (a -> b) where
    fromOObj (OFunc f) =  Just $ \input ->
        let
            oInput = toOObj input
            oOutput = f oInput
            output = fromOObj oOutput :: Maybe b
        in case output of
            Just out -> out
            Nothing -> error $ "coercing OVal to a -> b isn't always safe; use a -> Maybe b"
                          ++ " (trace: " ++ show oInput ++ " -> " ++ show oOutput ++ " )"
    fromOObj _ = Nothing
    toOObj f = OFunc $ \oObj -> 
        case fromOObj oObj :: Maybe a of
            Nothing  -> OError ["bad input type"]
            Just obj -> toOObj $ f obj


instance forall a b. (OTypeMirror a, OTypeMirror b) => OTypeMirror (Either a b) where
    fromOObj (fromOObj -> Just (x :: a)) = Just $ Left  x
    fromOObj (fromOObj -> Just (x :: b)) = Just $ Right x
    fromOObj _ = Nothing

    toOObj (Right x) = toOObj x
    toOObj (Left  x) = toOObj x

oTypeStr :: OVal -> [Char]
oTypeStr (OUndefined) = "Undefined"
oTypeStr (OBool   _ ) = "Bool"
oTypeStr (ONum    _ ) = "Number"
oTypeStr (OList   _ ) = "List"
oTypeStr (OString _ ) = "String"
oTypeStr (OFunc   _ ) = "Function"
oTypeStr (OModule _ ) = "Module"
oTypeStr (OError  _ ) = "Error"

getErrors :: OVal -> Maybe String
getErrors (OError er) = Just $ head er
getErrors (OList l)   = Monad.msum $ map getErrors l
getErrors _           = Nothing


type Any = OVal

caseOType = flip ($)

infixr 2 <||>

(<||>) :: forall desiredType out. (OTypeMirror desiredType)
    => (desiredType -> out) 
    -> (OVal -> out)
    -> (OVal -> out)

(<||>) f g = \input ->
    let
        coerceAttempt = fromOObj input :: Maybe desiredType
    in 
        if isJust coerceAttempt -- ≅ (/= Nothing) but no Eq req
        then f $ (\(Just a) -> a) coerceAttempt
        else g input

divideObjs :: [OVal] -> ([SymbolicObj2], [SymbolicObj3], [OVal])
divideObjs children = 
    (map fromOObj2 . filter isOObj2 $ children,
     map fromOObj3 . filter isOObj3 $ children,
     filter (not . isOObj)          $ children)
        where
            isOObj2 (OObj2 _) = True
            isOObj2    _      = False
            isOObj3 (OObj3 _) = True
            isOObj3    _      = False
            isOObj  (OObj2 _) = True
            isOObj  (OObj3 _) = True
            isOObj     _      = False
            fromOObj2 (OObj2 x) = x
            fromOObj3 (OObj3 x) = x


