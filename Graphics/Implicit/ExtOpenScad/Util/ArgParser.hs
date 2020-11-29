-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is this required?
{-# LANGUAGE ScopedTypeVariables #-}

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Util.ArgParser (argument, doc, defaultTo, example, test, eulerCharacteristic, argMap) where

-- imported twice, once qualified. null from Data.Map conflicts with null from Prelude.
import Prelude(String, Maybe(Just, Nothing), ($), (<>), show, return, fmap, snd, filter, (.), fst, foldl1, not, (&&), (<$>), maybe)
import qualified Prelude as P (null)

import Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP, APTest, APBranch, APTerminator, APFail, APExample), OVal (OError), TestInvariant(EulerCharacteristic), Symbol, VarLookup(VarLookup))

import Graphics.Implicit.ExtOpenScad.Util.OVal (fromOObj, toOObj, OTypeMirror)

import Graphics.Implicit.Definitions(ℕ)

-- imported twice, once qualified. null from Data.Map conflicts with null from Prelude.
import Data.Map (fromList, lookup, delete)
import qualified Data.Map as DM (null)

import Data.Maybe (isNothing, fromJust, isJust)

import Data.Text.Lazy (Text, pack, unpack)

import Control.Arrow (first)

-- * ArgParser building functions

-- ** argument and combinators

-- | Builds an argparser for the type that is expected from it.
--   FIXME: make a version of this that accepts multiple symbol names, so we can have h= and height=
argument :: forall desiredType. (OTypeMirror desiredType) => Symbol -> ArgParser desiredType
argument name =
    AP name Nothing "" $ \oObjVal -> do
        let
            val :: Maybe desiredType
            val = fromOObj oObjVal
            errmsg :: Text
            errmsg = case oObjVal of
                OError err -> "error in computing value for argument " <> pack (show name)
                              <> ": " <>  err
                _   ->  "arg " <> pack (show oObjVal) <> " not compatible with " <> pack (show name)
        maybe (APFail errmsg) APTerminator val
{-# INLINABLE argument #-}

-- | Inline documentation.
doc :: forall a. ArgParser a -> Text -> ArgParser a
doc (AP name defMaybeVal _ next) newDoc = AP name defMaybeVal newDoc next
doc _ _ = APFail "Impossible! doc"

-- | An inline default value.
defaultTo :: forall a. (OTypeMirror a) => ArgParser a -> a -> ArgParser a
defaultTo (AP name _ doc' next) newDefVal =
    AP name (Just $ toOObj newDefVal) doc' next
defaultTo _ _ = APFail "Impossible! defaultTo"

-- | An inline example.
example :: Text -> ArgParser ()
example str = APExample str (return ())

-- | Inline test and combinators.
test :: Text -> ArgParser ()
test str = APTest str [] (return ())

eulerCharacteristic :: ArgParser a -> ℕ -> ArgParser a
eulerCharacteristic (APTest str tests child) χ =
    APTest str (EulerCharacteristic χ : tests) child
eulerCharacteristic _ _ = APFail "Impossible! eulerCharacteristic"

-- * Tools for handeling ArgParsers

-- | Apply arguments to an ArgParser
argMap ::
    [(Maybe Symbol, OVal)]      -- ^ arguments
    -> ArgParser a              -- ^ ArgParser to apply them to
    -> (Maybe a, [String])      -- ^ (result, error messages)
argMap args = argMap2 unnamedArgs (VarLookup $ fromList namedArgs) where
    unnamedArgs = snd <$> filter (isNothing . fst) args
    namedArgs   = first fromJust <$> filter (isJust . fst) args

argMap2 :: [OVal] -> VarLookup -> ArgParser a -> (Maybe a, [String])
argMap2 unnamedArgs namedArgs (APBranch branches) =
    foldl1 merge solutions where
        solutions = fmap (argMap2 unnamedArgs namedArgs) branches
        merge :: forall a. (Maybe a, [String]) -> (Maybe a, [String]) -> (Maybe a, [String])
        merge a@(Just _, []) _ = a
        merge _ b@(Just _, []) = b
        merge a@(Just _, _) _ = a
        merge (Nothing, _)  a = a

-- FIXME: don't use delete directly here, wrap it in StateC.hs
-- FIXME: generate a warning.
argMap2 unnamedArgs (VarLookup namedArgs) (AP name fallback _ f) =
    case lookup name namedArgs of
        Just a -> argMap2
            unnamedArgs
            (VarLookup $ delete name namedArgs)
            (f a)
        Nothing -> case unnamedArgs of
            x:xs -> argMap2 xs (VarLookup namedArgs) (f x)
            []   -> case fallback of
                Just b  -> argMap2 [] (VarLookup namedArgs) (f b)
                Nothing -> (Nothing, ["No value and no default for argument " <> show name])

-- FIXME: don't use map.null here, wrap it in StateC.hs.
-- FIXME: generate a warning.
argMap2 a (VarLookup b) (APTerminator val) =
    (Just val, ["Unused arguments" | not (P.null a && DM.null b)])

argMap2 _ _ (APFail err) = (Nothing, [unpack err])

argMap2 a b (APExample _ child) = argMap2 a b child

argMap2 a b (APTest _ _ child) = argMap2 a b child
