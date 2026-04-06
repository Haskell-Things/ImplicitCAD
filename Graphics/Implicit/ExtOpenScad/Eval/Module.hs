-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

-- Utility functions for handling module calling.
module Graphics.Implicit.ExtOpenScad.Eval.Module (
  checkInstances,
  checkOptions,
  ensureNoSuite,
  nameOfModule,
  runModule,
  ) where

import Prelude(Maybe(Just, Nothing), Bool(False), (.), ($), elem, error, filter, fmap, fst, init, last, length, not, notElem, null, show, snd, pure, zip, (<>), (&&), (==), (/=), String, (<$>))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Expr(),
                                                  OVal(OUModule, ONModule, ONModuleWithSuite, OVargsModule),
                                                  SourcePosition,
                                                  StateC,
                                                  StatementI,
                                                  Symbol(Symbol)
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.StateC (errorC)

import qualified Data.List as DL (intercalate)

import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)

import Control.Monad (when)

import Data.Foldable (for_)

import Data.Traversable (for)

import Data.Text.Lazy as DTL (concat, intercalate)

import Data.Text.Lazy (pack, Text)

-- | Ensure that argsExpr fits into args.
checkOptions :: Maybe [(Symbol, Bool)] -> [(Maybe Symbol, Expr)] -> Bool -> SourcePosition -> StateC Bool
checkOptions args argsExpr makeWarnings sourcePos = do
  let
    -- Find what arguments are satisfied by a default value, were given in a named parameter, or were given.. and count them.
    valDefaulted ,valNotDefaulted, valNamed, mappedDefaulted, mappedNotDefaulted, notMappedNotDefaultable :: [Symbol]
    -- function definition has a default value.
    valDefaulted  = fmap fst $ filter snd $ fromMaybe [] args
    -- function definition has no default value.
    valNotDefaulted = fmap fst $ filter (not.snd) $ fromMaybe [] args
    -- function call has a named expression bound to this symbol.
    valNamed = namedParameters argsExpr
    -- function call has a named expression, function definition has an argument with this name, AND there is a default value for this argument.
    mappedDefaulted = filter (`elem` valNamed) valDefaulted
    -- function call has a named expression, function definition has an argument with this name, AND there is NOT a default value for this argument.
    mappedNotDefaulted = filter (`elem` valNamed) valNotDefaulted
    -- arguments we need to find a mapping for, from the unnamed expressions.
    notMappedNotDefaultable = filter (`notElem` mappedNotDefaulted) valNotDefaulted
    -- expressions without a name.
    valUnnamed :: [Expr]
    valUnnamed = unnamedParameters argsExpr
    mapFromUnnamed :: [(Symbol, Expr)]
    mapFromUnnamed = zip notMappedNotDefaultable valUnnamed
    missingNotDefaultable = filter (`notElem` (mappedDefaulted <> mappedNotDefaulted <> fmap fst mapFromUnnamed)) valNotDefaulted
    extraUnnamed = filter (`notElem` (valDefaulted <> valNotDefaulted)) $ namedParameters argsExpr
    namedParameters :: [(Maybe Symbol, Expr)] -> [Symbol]
    namedParameters = mapMaybe fst
    unnamedParameters :: [(Maybe Symbol, Expr)] -> [Expr]
    unnamedParameters = mapMaybe (
      \(argName, expr) ->
        case argName of
          Just _  -> Nothing
          Nothing -> Just expr
      )
    parameterReport =  "Passed " <>
                       (if null valNamed && null valUnnamed then "no parameters" else "" ) <>
                       (if not (null valNamed) then show (length valNamed) <> (if length valNamed == 1 then " named parameter" else " named parameters") else "" ) <>
                       (if not (null valNamed) && not (null valUnnamed) then ", and " else "") <>
                       (if not (null valUnnamed) then show (length valUnnamed) <> (if length valUnnamed == 1 then " un-named parameter." else " un-named parameters.") else ".") <>
                       (if not (null missingNotDefaultable) then
                           (if length missingNotDefaultable == 1
                            then " Couldn't match one parameter: " <> showSymbol (last missingNotDefaultable)
                            else " Couldn't match " <> show (length missingNotDefaultable) <> " parameters: " <> DL.intercalate ", " (showSymbol <$> init missingNotDefaultable) <> " and " <> showSymbol (last missingNotDefaultable) <> "."
                           ) else "") <>
                       (if not (null extraUnnamed)
                        then
                          (if length extraUnnamed == 1
                           then " Had one extra parameter: " <> showSymbol (last extraUnnamed)
                           else " Had " <> show (length extraUnnamed) <> " extra parameters. They are:" <> DL.intercalate ", " (showSymbol <$> init extraUnnamed) <> " and " <> showSymbol (last extraUnnamed) <> "."
                          )
                        else "")
    showSymbol :: Symbol -> String
    showSymbol (Symbol sym) = show sym
  when (not (null missingNotDefaultable) && makeWarnings)
    (errorC sourcePos $ "Insufficient parameters. " <> pack parameterReport)
  when (not (null extraUnnamed) && isJust args && makeWarnings)
    (errorC sourcePos $ "Too many parameters: " <> pack (show $ length extraUnnamed) <> " extra. " <> pack parameterReport)
  pure $ null missingNotDefaultable && null extraUnnamed

-- | Do not evaluate the suite, if there is one. Throw an error instead.
ensureNoSuite :: SourcePosition -> OVal -> [StatementI] -> StateC [OVal]
ensureNoSuite sourcePos mod suite = do
  when (suite /= []) (errorC sourcePos $ "Suite provided, but module " <> nameOfModule mod <> " does not accept one. Perhaps a missing semicolon?")
  pure []

-- | Check the instances, make sure we can only resolve one instance, or throw a warning.
checkInstances :: SourcePosition -> OVal -> [(Maybe Symbol, Expr)] -> [[(Symbol, Bool)]] -> StateC ()
checkInstances sourcePos mod argsExpr forms = do
  possibleInstances <- selectInstances forms argsExpr sourcePos
  when (null possibleInstances) (errorC sourcePos $ "No instance of " <> nameOfModule mod <> " found to match given parameters.\narguments given:\n" <> pack (show argsExpr) <> "\nForms available:" <> pack (show forms) <> "\n")
-- FIXME: make this a warning that can be turned on and off, and is off by default.
{-
  when (length possibleInstances > 1) (warnC sourcePos $ "Multiple instances of " <> nameOfModule mod <> " found matching given parameters.\nInstances found:\n" <> (DTL.concat $ showInstance mod <$> possibleInstances) <> "Parameters given: " <> nameOfModule mod <> "(" <> (DTL.intercalate ", " $ showParameter <$> argsExpr) <> ");")
  where
    showParameter :: (Maybe Symbol, Expr) -> Text
    showParameter (Just (Symbol s), v) = s <> "=" <> pack (show v)
    showParameter (Nothing, LitE v) = pack (show v)
    showParameter (Nothing, v) = pack (show v)
    showInstance :: OVal -> [(Symbol, Bool)] -> Text
    showInstance myMod args = nameOfModule myMod <> "(" <> (DTL.intercalate "," $ showArg <$> args) <> ");\n"
    showArg :: (Symbol, Bool) -> Text
    showArg (Symbol argName, optional) = argName <> "=...(" <> (if optional then "optional)" else "required)")
-}

-- Run a module.
runModule :: SourcePosition -> (Maybe (StateC [OVal]), [String]) -> StateC [OVal]
runModule sourcePos argsMapped = do
  for_ (pack <$> snd argsMapped) $ errorC sourcePos
  fromMaybe (pure []) (fst argsMapped)

selectInstances :: [[(Symbol, Bool)]] -> [(Maybe Symbol, Expr)] -> SourcePosition -> StateC [[(Symbol, Bool)]]
selectInstances instances argsExpr sourcePos = do
  validInstances <- for instances
                    ( \args -> do
                        res <- checkOptions (Just args) argsExpr False sourcePos
                        pure $ if res then Just args else Nothing
                    )
  pure $ catMaybes validInstances

-- Find the name of a module.
nameOfModule :: OVal -> Text
nameOfModule mod = case mod of
  (OUModule (Symbol modName) _ _) -> modName
  (ONModule (Symbol modName) _ _) -> modName
  (ONModuleWithSuite (Symbol modName) _ _) -> modName
  (OVargsModule (Symbol modName) _) -> modName
  _ -> error "Tried to get the name of a non-module."

