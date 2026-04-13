-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), (.), ($), show, pure, (<>), reverse, readFile, not, null, (*>), traverse, (<$))

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
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalArgs, evalExpr, matchPat)
import Graphics.Implicit.ExtOpenScad.Eval.Module (checkInstances, checkOptions, ensureNoSuite, runModule)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Data.Map (union, fromList, toList)

import Data.Maybe (isJust)

import Control.Monad (when, unless)

import Control.Monad.State (gets, liftIO)

import Data.Foldable (traverse_, for_)

import Data.Traversable (for)

import Data.Text.Lazy (unpack, pack)

import System.Directory (doesFileExist)

import System.FilePath (takeDirectory)
import Control.Monad.Reader.Class (MonadReader(ask))

-- | Run a single OpenSCAD statement.
runStatementI :: StatementI -> StateC ()
runStatementI (StatementI sourcePos (pat := expr)) = do
    -- Interpret variable assignment
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
            Just mod@(OUModule {}) -> ensureNoSuite sourcePos mod suite
            Just mod@(ONModule {}) -> ensureNoSuite sourcePos mod suite
            Just (ONModuleWithSuite {}) -> evalSuite varlookup sourcePos suite
            Just mod@(OVargsModule {}) -> ensureNoSuite sourcePos mod suite
            _ -> pure []

          -- Check that an instance exists that can execute the module, as it was called.
          case maybeMod of
            Just (OUModule {}) -> pure ()
            Just mod@(ONModule _ _ forms) -> checkInstances sourcePos mod argsExpr forms
            Just mod@(ONModuleWithSuite _ _ forms) -> checkInstances sourcePos mod argsExpr forms
            Just (OVargsModule {}) -> pure ()
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
              -- Run the module, which evaluates it's own suite, and cannot return anything.
              implementation modname sourcePos evaluatedArgs suite runSuite
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

-- Execute a set of OpenSCAD statements, without returning results.
runSuite :: [StatementI] -> StateC ()
runSuite = traverse_ runStatementI

-- | Evaluate the suite of an OpenSCAD module.
evalSuite :: VarLookup -> SourcePosition -> [StatementI] -> StateC [OVal]
evalSuite varlookup sourcePos suite = do
  vals <- runSuiteCapture varlookup suite
  when (null vals) (errorC sourcePos "Suite required, but none provided.")
  runSuiteCapture varlookup suite

-- | Execute the suite of an OpenSCAD module.
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

