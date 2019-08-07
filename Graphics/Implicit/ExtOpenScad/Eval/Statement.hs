-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is this required?
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), (.), ($), show, concatMap, return, (++), reverse, fst, snd, readFile, filter, length, lookup, (+), (<), (||), (>), (&&), (==))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Statement(Include, (:=), Echo, For, If, NewModule, ModuleCall, DoNothing),
                                                  Pattern(Name),
                                                  Expr(LitE),
                                                  OVal(OString, OBool, OList, OModule, OUModule),
                                                  VarLookup(VarLookup),
                                                  StatementI(StatementI),
                                                  Symbol(Symbol),
                                                  Message(Message),
                                                  MessageType(TextOut),
                                                  ScadOpts(ScadOpts),
                                                  StateC,
                                                  CompState(CompState)
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.OVal (getErrors)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser (argument, defaultTo, argMap)
import Graphics.Implicit.ExtOpenScad.Util.StateC (errorC, warnC, modifyVarLookup, mapMaybeM, scadOptions, lookupVar, pushVals, getRelPath, withPathShiftedBy, getVals, putVals, addMessage, getVarLookup)
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Data.Map (union, fromList, toList)

import Data.Maybe (isJust, fromMaybe)

import Control.Monad (forM_, forM, mapM_, when)

import Control.Monad.State (get, liftIO, mapM, runStateT, (>>))

import System.FilePath (takeDirectory)

-- helper, to use union on VarLookups.
varUnion :: VarLookup -> VarLookup -> VarLookup
varUnion (VarLookup a) (VarLookup b) = VarLookup $ union a b

-- Run statements out of the OpenScad file.
runStatementI :: StatementI -> StateC ()

-- | Interpret variable assignment
runStatementI (StatementI sourcePos (pat := expr)) = do
    val <- evalExpr sourcePos expr
    let posMatch = matchPat pat val
    case (getErrors val, posMatch) of
        (Just err,  _ ) -> errorC sourcePos err
        (_, Just (VarLookup match)) ->
          forM_ (toList match) $ \(Symbol varName, _) -> do
            maybeVar <- lookupVar (Symbol varName)
            when (isJust maybeVar)
              (warnC sourcePos $ "redefining already defined variable: " ++ show varName)
            modifyVarLookup $ varUnion (VarLookup match)
        (_,   Nothing ) -> errorC sourcePos "pattern match failed in assignment"

-- | Interpret an echo statement.
runStatementI (StatementI sourcePos (Echo exprs)) = do
    let
        show2 (OString s) = s
        show2 x = show x
    vals <- mapM (evalExpr sourcePos) exprs
    case getErrors (OList vals) of
        Nothing  -> addMessage TextOut sourcePos $ concatMap show2 vals
        Just err -> errorC sourcePos err

runStatementI (StatementI sourcePos (For pat expr loopContent)) = do
    val <- evalExpr sourcePos expr
    case (getErrors val, val) of
        (Just err, _)      -> errorC sourcePos err
        (_, OList vals) -> forM_ vals $ \v ->
            case matchPat pat v of
                Just match -> do
                    modifyVarLookup $ varUnion match
                    runSuite loopContent
                Nothing -> return ()
        _ -> return ()

runStatementI (StatementI sourcePos (If expr a b)) = do
    val <- evalExpr sourcePos expr
    case (getErrors val, val) of
        (Just err,  _  )  -> errorC sourcePos ("In conditional expression of if statement: " ++ err)
        (_, OBool True )  -> runSuite a
        (_, OBool False)  -> runSuite b
        _                 -> return ()

-- | Interpret a module declaration.
runStatementI (StatementI sourcePos (NewModule name argTemplate suite)) = do
    argTemplate' <- forM argTemplate $ \(argName, defexpr) -> do
        defval <- mapMaybeM (evalExpr sourcePos) defexpr
        return (argName, defval)
    argNames <-  forM argTemplate $ \(argName, defexpr) -> do
      defval <- mapMaybeM (evalExpr sourcePos) defexpr
      let
        hasDefault = isJust defval
      return (argName, hasDefault)
    (VarLookup varlookup) <- getVarLookup
--  FIXME: \_? really?
    runStatementI . StatementI sourcePos $ (Name name :=) $ LitE $ OUModule name (Just argNames) $ \_ -> do
        newNameVals <- forM argTemplate' $ \(argName, maybeDef) -> do
            val <- case maybeDef of
                Just def -> argument argName `defaultTo` def
                Nothing  -> argument argName
            return (argName, val)
        let
            varlookup' = union (fromList newNameVals) varlookup
        return $ runSuiteCapture (VarLookup varlookup') suite

-- | Interpret a call to a module.
runStatementI (StatementI sourcePos (ModuleCall (Symbol name) argsExpr suite)) = do
        maybeMod <- lookupVar (Symbol name)
        varlookup <- getVarLookup
        newVals  <- case maybeMod of
            Just (OUModule _ args mod') -> do
              -- Find what arguments are satisfied by a named parameter.
              valNamed <- forM argsExpr $ \(argName, _) ->
                case argName of
                  Just (Symbol symbol) ->
                    case lookup (Symbol symbol) (fromMaybe [] args) of
                      Just _  -> do
                        maybeVar <- lookupVar (Symbol symbol)
                        when (isJust maybeVar)
                          (warnC sourcePos $ "Supplied parameter shadows variable from the calling scope: " ++ symbol)
                        return (Just (Symbol symbol), True)
                      Nothing -> do
                        -- FIXME: maybe is a workaround here.
                        when (isJust args)
                          (warnC sourcePos $ "Supplied parameter not listed in module declaration: " ++ symbol)
                        return (Nothing, False)
                  Nothing -> return (Nothing, False)
              -- Find what arguments are satisfied by a default value, or were given in a named parameter..
              valFound <- forM argsExpr $ \(argName, _) ->
                case argName of
                  Just symbol  -> do
                    let supplied = fromMaybe False $ lookup (Just symbol) valNamed
                    case lookup symbol (fromMaybe [] args) of
                      Just hasDefault -> return (Just symbol, hasDefault || supplied)
                      Nothing         -> if supplied
                                         then return (Just symbol, supplied)
                                         else return (Nothing, False)
                  Nothing -> return (Nothing, False)
              -- Find what unnamed parameters were passed in.
              valUnnamed <- forM argsExpr $ \(argName, expr) ->
                case argName of
                  Just _  -> return Nothing
                  Nothing -> return $ Just expr
              let
                -- ... and count them.
                valNamedCount = length $ filter snd valNamed
                valFoundCount = length $ filter snd valFound
                valUnnamedCount = length $ filter isJust valUnnamed
                noArgs = length (fromMaybe [] args)
                parameterReport =  "Found " ++ show valNamedCount ++
                  (if valNamedCount == 1 then " named parameter, and " else " named parameters, and ")
                   ++ show valUnnamedCount ++
                  (if valUnnamedCount == 1 then " un-named parameter. Expected " else " un-named parameters. Expected ")
                  ++ show noArgs ++
                  (if noArgs == 1 then " parameter." else " parameters.")
              -- Check that the number of arguments match, and if they don't, error a report.
              -- FIXME: take into account the ordering of unnamed value application.
              when (valFoundCount + valUnnamedCount < noArgs)
                (errorC sourcePos $ "Insufficient parameters. " ++ parameterReport)
              when (valFoundCount + valUnnamedCount > noArgs && isJust args)
                (errorC sourcePos $ "Too many parameters. " ++ parameterReport)
              -- Evaluate all of the arguments.
              argsVal <- forM argsExpr $ \(posName, expr) -> do
                val <- evalExpr sourcePos expr
                return (posName, val)
              -- Run the function.
              childVals <- runSuiteCapture varlookup suite
              let
                argsMapped = argMap argsVal $ mod' childVals
              forM_ (snd argsMapped) $ errorC sourcePos
              fromMaybe (return []) (fst argsMapped)
            Just (OModule _ _ mod') -> do
              -- Evaluate all of the arguments.
              argsVal <- forM argsExpr $ \(posName, expr) -> do
                val <- evalExpr sourcePos expr
                return (posName, val)
              -- Run the function.
              childVals <- runSuiteCapture varlookup suite
              let
                argsMapped = argMap argsVal $ mod' childVals
                ioNewVals = fromMaybe (return []) (fst argsMapped)
              forM_ (snd argsMapped) $ errorC sourcePos
              liftIO ioNewVals
            Just foo -> do
                    case getErrors foo of
                        Just err -> errorC sourcePos err
                        Nothing  -> errorC sourcePos $ "Object " ++ name ++ " is not a module!"
                    return []
            Nothing -> do
                errorC sourcePos $ "Module " ++ name ++ " not in scope."
                return []
        pushVals newVals

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
        Left e -> errorC sourcePos $ "Error parsing " ++ name ++ ":" ++ show e
        Right sts -> withPathShiftedBy (takeDirectory name) $ do
            vals <- getVals
            putVals []
            runSuite sts
            vals' <- getVals
            if injectVals then putVals (vals' ++ vals) else putVals vals
      else
        warnC sourcePos $ "Not importing " ++ name ++ ": File import disabled."

runStatementI (StatementI _ DoNothing) = return ()

runSuite :: [StatementI] -> StateC ()
runSuite = mapM_ runStatementI

runSuiteCapture :: VarLookup -> [StatementI] -> StateC [OVal]
runSuiteCapture varlookup suite = do
    (CompState (_ , _, path, _, opts)) <- get
    (res, CompState (_, _, _, messages, _)) <- liftIO $ runStateT
        (runSuite suite >> getVals)
        (CompState (varlookup, [], path, [], opts))
    let
      moveMessage (Message mtype mpos text) = addMessage mtype mpos text
    mapM_ moveMessage messages
    return $ reverse res
