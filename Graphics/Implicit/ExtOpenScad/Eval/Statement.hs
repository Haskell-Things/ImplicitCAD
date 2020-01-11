-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is this required?
{-# LANGUAGE ScopedTypeVariables #-}

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), (.), ($), show, pure, (<>), reverse, fst, snd, readFile, filter, length, (&&), (==), (/=), fmap, notElem, elem, not, zip, init, last, null, String, (*>), (<$>), traverse)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Statement(Include, (:=), If, NewModule, ModuleCall, DoNothing),
                                                  Pattern(Name),
                                                  Expr(LitE),
                                                  OVal(OBool, OUModule, ONModule, OVargsModule),
                                                  VarLookup(VarLookup),
                                                  StatementI(StatementI),
                                                  Symbol(Symbol),
                                                  Message(Message),
                                                  ScadOpts(ScadOpts),
                                                  StateC,
                                                  CompState(CompState),
                                                  varUnion
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

import "monads-tf" Control.Monad.State (get, liftIO, runStateT)

import Data.Foldable (traverse_, for_)

import Data.Traversable (for)

import System.FilePath (takeDirectory)

-- Run statements out of the OpenScad file.
runStatementI :: StatementI -> StateC ()

-- | Interpret variable assignment
runStatementI (StatementI sourcePos (pat := expr)) = do
    val <- evalExpr sourcePos expr
    let posMatch = matchPat pat val
    case (getErrors val, posMatch) of
        (Just err,  _ ) -> errorC sourcePos err
        (_, Just (VarLookup match)) ->
          for_ (toList match) $ \(Symbol varName, _) -> do
            maybeVar <- lookupVar (Symbol varName)
            when (isJust maybeVar)
              (warnC sourcePos $ "redefining already defined object: " <> show varName)
            modifyVarLookup $ varUnion (VarLookup match)
        (_,   Nothing ) -> errorC sourcePos "pattern match failed in assignment"

-- | Interpret an if conditional statement.
runStatementI (StatementI sourcePos (If expr a b)) = do
    val <- evalExpr sourcePos expr
    case (getErrors val, val) of
        (Just err,  _  )  -> errorC sourcePos ("In conditional expression of if statement: " <> err)
        (_, OBool True )  -> runSuite a
        (_, OBool False)  -> runSuite b
        _                 -> pure ()

-- | Interpret a module declaration.
runStatementI (StatementI sourcePos (NewModule name argTemplate suite)) = do
    argTemplate' <- for argTemplate $ \(argName, defexpr) -> do
        defval <- traverse (evalExpr sourcePos) defexpr
        pure (argName, defval)
    argNames <-  for argTemplate $ \(argName, defexpr) -> do
      defval <- traverse (evalExpr sourcePos) defexpr
      let
        hasDefault = isJust defval
      pure (argName, hasDefault)
    (VarLookup varlookup) <- getVarLookup
--  FIXME: \_? really?
    runStatementI . StatementI sourcePos $ (Name name :=) $ LitE $ OUModule name (Just argNames) $ \_ -> do
        newNameVals <- for argTemplate' $ \(argName, maybeDef) -> do
            val <- case maybeDef of
                Just def -> argument argName `defaultTo` def
                Nothing  -> argument argName
            pure (argName, val)
        let
            varlookup' = union (fromList newNameVals) varlookup
        pure $ runSuiteCapture (VarLookup varlookup') suite

-- | Interpret a call to a module.
runStatementI (StatementI sourcePos (ModuleCall (Symbol name) argsExpr suite)) = do
        maybeMod <- lookupVar (Symbol name)
        varlookup <- getVarLookup
        newVals  <- case maybeMod of
            Just (OUModule _ args mod') -> do
              optionsMatch <- checkOptions args True
              unless optionsMatch (errorC sourcePos $ "Options check failed when executing user-defined module " <> name <> ".")
              evaluatedArgs <- evalArgs argsExpr
              -- Evaluate the suite.
              suiteResults <- runSuiteCapture varlookup suite
              when (suite /= []) (errorC sourcePos $ "Suite provided, but module " <> name <> " does not accept one. Perhaps a missing semicolon?")
              -- Run the module.
              let
                argsMapped = argMap evaluatedArgs $ mod' suiteResults
              for_ (snd argsMapped) $ errorC sourcePos
              fromMaybe (pure []) (fst argsMapped)
            Just (ONModule _ implementation forms) -> do
              possibleInstances <- selectInstances forms
              let
                suiteInfo = case possibleInstances of
                  [(_, suiteInfoFound)]   -> suiteInfoFound
                  []                      -> Nothing
                  ((_, suiteInfoFound):_) -> suiteInfoFound
              when (null possibleInstances) (do
                                                errorC sourcePos $ "no instance of " <> name <> " found to match given parameters.\nInstances available:\n" <> show (ONModule (Symbol name) implementation forms)
                                                traverse_ (`checkOptions` True) $ fmap (Just . fst) forms
                                            )
              -- Ignore this for now, because all instances we define have the same suite requirements.
              {-
              when (length possibleInstances > 1) (do
                                                      errorC sourcePos $ "too many instances of " <> name <> " have been found that match given parameters."
                                                      traverse_ (`checkOptions` True) $ fmap (Just . fst) possibleInstances)
              -}
              -- Evaluate all of the arguments.
              evaluatedArgs <- evalArgs argsExpr
              -- Evaluate the suite.
              vals <- runSuiteCapture varlookup suite
              suiteResults <- case suiteInfo of
                              Just True -> do
                                when (null vals) (errorC sourcePos "Suite required, but none provided.")
                                pure vals
                              Just False -> pure vals
                              _ -> do
                                when (suite /= []) (errorC sourcePos $ "Suite provided, but module " <> name <> " does not accept one. Perhaps a missing semicolon?")
                                pure []
              -- Run the module.
              let
                argsMapped = argMap evaluatedArgs $ implementation sourcePos suiteResults
              for_ (snd argsMapped) $ errorC sourcePos
              fromMaybe (pure []) (fst argsMapped)
            Just (OVargsModule modname mod') -> do
              -- Evaluate all of the arguments.
              evaluatedArgs <- evalArgs argsExpr
              -- Run the module, which evaluates it's own suite.
              _ <- mod' modname sourcePos evaluatedArgs suite runSuite -- no values are pureed
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
          where
            selectInstances :: [([(Symbol, Bool)], Maybe Bool)] -> StateC [([(Symbol, Bool)], Maybe Bool)]
            selectInstances instances = do
              validInstances <- for instances
                    ( \(args, suiteInfo) -> do
                        res <- checkOptions (Just args) False
                        pure $ if res then Just (args, suiteInfo) else Nothing
                    )
              pure $ catMaybes validInstances
            checkOptions :: Maybe [(Symbol, Bool)] -> Bool -> StateC Bool
            checkOptions args makeWarnings = do
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
                  (if not (null extraUnnamed) then
                      (if length extraUnnamed == 1
                       then " Had one extra parameter: " <> showSymbol (last extraUnnamed)
                       else " Had " <> show (length extraUnnamed) <> " extra parameters. They are:" <> intercalate ", " (showSymbol <$> init extraUnnamed) <> " and " <> showSymbol (last extraUnnamed) <> "."
                      ) else "")
                showSymbol :: Symbol -> String
                showSymbol (Symbol sym) = show sym
                  {-
              when (makeWarnings)
                (errorC sourcePos $ foldMap show argsExpr)
              when (makeWarnings)
                (errorC sourcePos $ "valNamed: " <> show (length valNamed))
              when (makeWarnings)
                (errorC sourcePos $ "mappedDefaulted: " <> show (length mappedDefaulted))
              when (makeWarnings)
                (errorC sourcePos $ "mappedNotDefaulted: " <> show (length mappedNotDefaulted))
              when (makeWarnings)
                (errorC sourcePos $ "notMappedNotDefaultable: " <> show (length notMappedNotDefaultable))
              when (makeWarnings)
                (errorC sourcePos $ "mapFromUnnamed: " <> show (length mapFromUnnamed))
              when (makeWarnings)
                (errorC sourcePos $ "missingNotDefaultable: " <> show (length missingNotDefaultable))
                 -}
              when (not (null missingNotDefaultable) && makeWarnings)
                (errorC sourcePos $ "Insufficient parameters. " <> parameterReport)
              when (not (null extraUnnamed) && isJust args && makeWarnings)
                (errorC sourcePos $ "Too many parameters: " <> show (length extraUnnamed) <> " extra. " <> parameterReport)
              pure $ null missingNotDefaultable && null extraUnnamed
            namedParameters :: [(Maybe Symbol, Expr)] -> [Symbol]
            namedParameters = mapMaybe fst
            unnamedParameters :: [(Maybe Symbol, Expr)] -> [Expr]
            unnamedParameters = mapMaybe (
              \(argName, expr) ->
                case argName of
                  Just _  -> Nothing
                  Nothing -> Just expr
              )
            evalArgs :: [(Maybe Symbol, Expr)] -> StateC [(Maybe Symbol, OVal)]
            evalArgs args = for args $ \(posName, expr) -> do
              val <- evalExpr sourcePos expr
              pure (posName, val)

-- | Interpret an include or use statement.
runStatementI (StatementI sourcePos (Include name injectVals)) = do
    scadOpts <- scadOptions
    let
      allowInclude :: ScadOpts -> Bool
      allowInclude (ScadOpts _ allow) = allow
    if allowInclude scadOpts
      then do
      name' <- getRelPath name
      content <- liftIO $ readFile name'
      case parseProgram name' content of
        Left e -> errorC sourcePos $ "Error parsing " <> name <> ":" <> show e
        Right sts -> withPathShiftedBy (takeDirectory name) $ do
            vals <- getVals
            putVals []
            runSuite sts
            if injectVals
              then do
                vals' <- getVals
                putVals (vals' <> vals)
              else putVals vals
      else warnC sourcePos $ "Not importing " <> name <> ": File import disabled."

runStatementI (StatementI _ DoNothing) = pure ()

runSuite :: [StatementI] -> StateC ()
runSuite = traverse_ runStatementI

runSuiteCapture :: VarLookup -> [StatementI] -> StateC [OVal]
runSuiteCapture varlookup suite = do
    (CompState (_ , _, path, _, opts)) <- get
    (res, CompState (_, _, _, messages, _)) <- liftIO $ runStateT
        (runSuite suite *> getVals)
        (CompState (varlookup, [], path, [], opts))
    let
      moveMessage (Message mtype mpos text) = addMessage mtype mpos text
    traverse_ moveMessage messages
    pure $ reverse res
