-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), (>), (.), ($), error, show, pure, (<>), reverse, fst, snd, readFile, filter, length, (&&), (==), (/=), fmap, notElem, elem, not, zip, init, last, null, String, (*>), (<$>), traverse, (<$))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  SourcePosition,
                                                  Statement(Include, (:=), If, NewModule, ModuleCall, DoNothing),
                                                  Pattern(Name),
                                                  Expr(LitE),
                                                  OVal(OBool, OUModule, ONModule, ONModuleWithSuite, OVargsModule),
                                                  VarLookup(VarLookup),
                                                  StatementI(StatementI),
                                                  Symbol(Symbol),
                                                  Message(Message),
                                                  ScadOpts(importsAllowed),
                                                  StateC,
                                                  CompState(CompState, sourceDir),
                                                  varUnion, runImplicitCadM
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.OVal (getErrors)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser (argument, defaultTo, argMap)
import Graphics.Implicit.ExtOpenScad.Util.StateC (errorC, warnC, modifyVarLookup, scadOptions, lookupVar, pushVals, getRelPath, withPathShiftedBy, getVals, putVals, addMessage, getVarLookup)
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Data.List (intercalate)

import Data.Map (union, fromList, toList)

import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)

import Control.Monad (when, unless)

import Control.Monad.State (gets, liftIO)

import Data.Foldable (traverse_, for_)

import Data.Traversable (for)

import Data.Text.Lazy (unpack, pack, Text)

import System.Directory (doesFileExist)

import System.FilePath (takeDirectory)
import Control.Monad.Reader.Class (MonadReader(ask))

-- | Run statements out of the OpenScad file.
runStatementI :: StatementI -> StateC ()
runStatementI (StatementI sourcePos (pat := expr)) = do
    -- Interpret variable assignment
    -- FIXME: instead of just expression evaluation, module calling?
    val <- evalExpr sourcePos expr
    let posMatch = matchPat pat val
    case (getErrors val, posMatch) of
        (Just err,  _ ) -> errorC sourcePos err
        (_, Just (VarLookup match)) ->
          for_ (toList match) $ \(Symbol varName, _) -> do
            maybeVar <- lookupVar (Symbol varName)
            when (isJust maybeVar)
              (warnC sourcePos $ "redefining already defined object: " <> varName)
            modifyVarLookup $ varUnion (VarLookup match)
        (_,   Nothing ) -> errorC sourcePos "pattern match failed in assignment"

runStatementI (StatementI sourcePos (If expr a b)) = do
    -- Interpret an if conditional statement.
    val <- evalExpr sourcePos expr
    case (getErrors val, val) of
        (Just err,  _  )  -> errorC sourcePos ("In conditional expression of if statement: " <> err)
        (_, OBool True )  -> runSuite a
        (_, OBool False)  -> runSuite b
        _                 -> pure ()

runStatementI (StatementI sourcePos (NewModule name argTemplate suite)) = do
    -- Interpret a module declaration.
    argTemplate' <- for argTemplate $ \(argName, defexpr) -> do
        defval <- traverse (evalExpr sourcePos) defexpr
        pure (argName, defval)
    argNames <-  for argTemplate $ \(argName, defexpr) -> do
      defval <- traverse (evalExpr sourcePos) defexpr
      let
        hasDefault = isJust defval
      pure (argName, hasDefault)
    runStatementI . StatementI sourcePos $ (Name name :=) $ LitE $ OUModule name (Just argNames) $ \(VarLookup varlookup) -> do
        newNameVals <- for argTemplate' $ \(argName, maybeDef) -> do
            val <- case maybeDef of
                Just def -> argument argName `defaultTo` def
                Nothing  -> argument argName
            pure (argName, val)
        let
            varlookup' = union (fromList newNameVals) varlookup
        pure $ runSuiteCapture (VarLookup varlookup') suite

runStatementI (StatementI sourcePos (ModuleCall (Symbol name) argsExpr suite)) = do
        -- Interpret a call to a module.
        maybeMod <- lookupVar (Symbol name)
        varlookup <- getVarLookup
        newVals  <- do
          -- Evaluate all of the arguments.
          evaluatedArgs <- evalArgs argsExpr sourcePos

          -- Evaluate the suites, if required.
          suiteResults <- case maybeMod of
                            Just mod@(OUModule _ _ _) -> ensureNoSuite sourcePos mod suite
                            Just mod@(ONModule _ _ _) -> ensureNoSuite sourcePos mod suite
                            Just (ONModuleWithSuite _ _ _) -> evalSuite varlookup sourcePos suite
                            Just mod@(OVargsModule _ _) -> ensureNoSuite sourcePos mod suite
                            _ -> pure []

          -- Check that an instance exists that can execute the module, as it was called.
          _ <- case maybeMod of
                 Just (OUModule _ _ _) -> pure ()
                 Just mod@(ONModule _ _ forms) -> checkInstances sourcePos mod argsExpr forms
                 Just mod@(ONModuleWithSuite _ _ forms) -> checkInstances sourcePos mod argsExpr forms
                 Just (OVargsModule _ _) -> pure ()
                 _ -> pure ()

          -- do any per-module-type work, and run the module.
          case maybeMod of
            Just (OUModule _ args implementation) -> do
              -- User modules can only have one instance, so we only have to check one set of options here.
              optionsMatch <- checkOptions args argsExpr True sourcePos
              unless optionsMatch (errorC sourcePos $ "Options check failed when executing user-defined module " <> name <> ".")
              varLookup <- getVarLookup
              -- Run the module.
              runModule sourcePos $ argMap evaluatedArgs $ implementation varLookup
            Just (ONModule _ implementation _) -> do
              -- Run the module.
              runModule sourcePos $ argMap evaluatedArgs $ implementation sourcePos
            Just (ONModuleWithSuite _ implementation _) -> do
              -- Run the module.
              runModule sourcePos $ argMap evaluatedArgs $ implementation sourcePos suiteResults
            Just (OVargsModule modname implementation) -> do
              -- Run the module, which evaluates it's own suite.
              _ <- implementation modname sourcePos evaluatedArgs suite runSuite -- no values are pureed
              pure []
            Just foo -> do
                    case getErrors foo of
                        Just err -> errorC sourcePos err
                        Nothing  -> errorC sourcePos $ "Object " <> name <> " is not a module!"
                    pure []
            _ -> do
                errorC sourcePos $ "Module " <> name <> " not in scope."
                pure []
        pushVals newVals

runStatementI (StatementI sourcePos (Include name injectVals)) = do
    -- Interpret an include or use statement.
    opts <- scadOptions
    if importsAllowed opts
      then do
      name' <- getRelPath (unpack name)
      hasFile <- liftIO $ doesFileExist name'
      if not hasFile
        then warnC sourcePos $ "Not importing " <> name <> ": File not found."
        else do
          content <- liftIO $ readFile name'
          case parseProgram name' content of
            Left e -> errorC sourcePos $ "Error parsing " <> name <> ":" <> pack (show e)
            Right sts -> withPathShiftedBy (takeDirectory $ unpack name) $ do
                vals <- getVals
                putVals []
                runSuite sts
                if injectVals
                  then do
                    vals' <- getVals
                    putVals $ vals' <> vals
                  else putVals vals
      else warnC sourcePos $ "Not importing " <> name <> ": File import disabled."

runStatementI (StatementI _ DoNothing) = pure ()

runSuite :: [StatementI] -> StateC ()
runSuite = traverse_ runStatementI

runSuiteCapture :: VarLookup -> [StatementI] -> StateC [OVal]
runSuiteCapture varlookup suite = do
  opts <- ask
  (res, messages, _) <- do
    s <- gets mkSubState
    liftIO . runImplicitCadM opts s $ runSuite suite *> getVals
  reverse res <$ traverse moveMessage messages
    where
      mkSubState s = CompState varlookup [] (sourceDir s)
      moveMessage (Message mtype mpos text) = addMessage mtype mpos text

selectInstances :: [[(Symbol, Bool)]] -> [(Maybe Symbol, Expr)] -> SourcePosition -> StateC [[(Symbol, Bool)]]
selectInstances instances argsExpr sourcePos = do
  validInstances <- for instances
                    ( \args -> do
                        res <- checkOptions (Just args) argsExpr False sourcePos
                        pure $ if res then Just args else Nothing
                    )
  pure $ catMaybes validInstances

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
                            else " Couldn't match " <> show (length missingNotDefaultable) <> " parameters: " <> intercalate ", " (showSymbol <$> init missingNotDefaultable) <> " and " <> showSymbol (last missingNotDefaultable) <> "."
                           ) else "") <>
                       (if not (null extraUnnamed)
                        then
                          (if length extraUnnamed == 1
                           then " Had one extra parameter: " <> showSymbol (last extraUnnamed)
                           else " Had " <> show (length extraUnnamed) <> " extra parameters. They are:" <> intercalate ", " (showSymbol <$> init extraUnnamed) <> " and " <> showSymbol (last extraUnnamed) <> "."
                          )
                        else "")
    showSymbol :: Symbol -> String
    showSymbol (Symbol sym) = show sym
  when (not (null missingNotDefaultable) && makeWarnings)
    (errorC sourcePos $ "Insufficient parameters. " <> pack parameterReport)
  when (not (null extraUnnamed) && isJust args && makeWarnings)
    (errorC sourcePos $ "Too many parameters: " <> pack (show $ length extraUnnamed) <> " extra. " <> pack parameterReport)
  pure $ null missingNotDefaultable && null extraUnnamed

-- Evaluate the arguments, turning them from expressions into values.
evalArgs :: [(Maybe Symbol, Expr)] -> SourcePosition -> StateC [(Maybe Symbol, OVal)]
evalArgs args sourcePos = for args $ \(posName, expr) -> do
  val <- evalExpr sourcePos expr
  pure (posName, val)

-- Do not evaluate the suite. throw an error instead.
ensureNoSuite :: SourcePosition -> OVal -> [StatementI] -> StateC [OVal]
ensureNoSuite sourcePos mod suite = do
  when (suite /= []) (errorC sourcePos $ "Suite provided, but module " <> nameOfModule mod <> " does not accept one. Perhaps a missing semicolon?")
  pure []

-- | Evaluate the suite.
evalSuite :: VarLookup -> SourcePosition -> [StatementI] -> StateC [OVal]
evalSuite varlookup sourcePos suite = do
  vals <- runSuiteCapture varlookup suite
  when (null vals) (errorC sourcePos "Suite required, but none provided.")
  runSuiteCapture varlookup suite

-- check the instances, make sure we can only resolve one instance.
checkInstances :: SourcePosition -> OVal -> [(Maybe Symbol, Expr)] -> [[(Symbol, Bool)]] -> StateC ()
checkInstances sourcePos mod argsExpr forms = do
  possibleInstances <- selectInstances forms argsExpr sourcePos
  when (null possibleInstances) (do
                                    errorC sourcePos $ "no instance of " <> nameOfModule mod <> " found to match given parameters.\nInstances available:\n" <> pack (show mod)
                                    traverse_ (\a -> checkOptions (Just a) argsExpr True sourcePos) forms)
  when (length possibleInstances > 1) (do
                                          errorC sourcePos $ "too many instances of " <> nameOfModule mod <> " have been found that match given parameters."
                                          traverse_ (\a -> checkOptions (Just a) argsExpr True sourcePos) possibleInstances)

-- Find the name of a module.
nameOfModule :: OVal -> Text
nameOfModule mod = case mod of
  (ONModule (Symbol modName) _ _) -> modName
  (ONModuleWithSuite (Symbol modName) _ _) -> modName
  _ -> error "Tried to get the name of a non-module."

-- Run a module.
runModule :: SourcePosition -> (Maybe (StateC [OVal]), [String]) -> StateC [OVal]
runModule sourcePos argsMapped = do
  for_ (pack <$> snd argsMapped) $ errorC sourcePos
  fromMaybe (pure []) (fst argsMapped)
