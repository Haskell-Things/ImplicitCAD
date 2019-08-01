-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is this required?
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), FilePath, IO, (.), ($), show, concatMap, return, (++), fmap, reverse, fst, readFile, filter, length, lookup, (+), (<), (||))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Statement(Include, (:=), Echo, For, If, NewModule, ModuleCall, DoNothing),
                                                  Pattern(Name),
                                                  Expr(LitE),
                                                  OVal(OString, OBool, OList, OModule),
                                                  VarLookup(VarLookup),
                                                  StatementI(StatementI),
                                                  Symbol(Symbol),
                                                  Message(Message),
                                                  MessageType(TextOut),
                                                  ScadOpts(ScadOpts)
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.OVal (getErrors)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser (argument, defaultTo, argMap)
import Graphics.Implicit.ExtOpenScad.Util.StateC (StateC, CompState(CompState), errorC, warnC, modifyVarLookup, mapMaybeM, scadOptions, lookupVar, pushVals, getRelPath, withPathShiftedBy, getVals, putVals, addMessage, getVarLookup)
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Data.Map (union, fromList)

import Data.Maybe (isJust, fromMaybe)

import Control.Monad (forM_, forM, mapM_)

import Control.Monad.State (get, liftIO, mapM, runStateT, (>>))

import System.FilePath (takeDirectory)

-- helper, to use union on VarLookups.
varUnion :: VarLookup -> VarLookup -> VarLookup
varUnion (VarLookup a) (VarLookup b) = VarLookup $ union a b

-- Run statements out of the OpenScad file.
runStatementI :: StatementI -> StateC ()

runStatementI (StatementI sourcePos (pat := expr)) = do
    val <- evalExpr sourcePos expr
    let posMatch = matchPat pat val
    case (getErrors val, posMatch) of
        (Just err,  _ ) -> errorC sourcePos err
        (_, Just match) -> modifyVarLookup $ varUnion match
        (_,   Nothing ) -> errorC sourcePos "pattern match failed in assignment"

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

runStatementI (StatementI sourcePos (NewModule name argTemplate suite)) = do
    argTemplate' <- forM argTemplate $ \(argName, defexpr) -> do
        defval <- mapMaybeM (evalExpr sourcePos) defexpr
        return (argName, defval)
    argNames <-  forM argTemplate $ \(argName, defexpr) -> do
      defval <- mapMaybeM (evalExpr sourcePos) defexpr
      let
        hasDefault = isJust defval
      return (argName, hasDefault)
    (CompState (VarLookup varlookup, _, path, _, scadOpts)) <- get
--  FIXME: \_? really?
    runStatementI . StatementI sourcePos $ (Name name :=) $ LitE $ OModule name (Just argNames) $ \_ -> do
        newNameVals <- forM argTemplate' $ \(argName, maybeDef) -> do
            val <- case maybeDef of
                Just def -> argument argName `defaultTo` def
                Nothing  -> argument argName
            return (argName, val)
        let
            varlookup' = union (fromList newNameVals) varlookup
            -- FIXME: what do we change to use runSuiteCapture here instead?
            suiteVals = runSuiteCaptureIO (VarLookup varlookup') path scadOpts suite
        return suiteVals

runStatementI (StatementI sourcePos (ModuleCall (Symbol name) argsExpr suite)) = do
        maybeMod  <- lookupVar (Symbol name)
        newVals <- case maybeMod of
            Just (OModule _ args mod') -> do
              -- Find what arguments are satisfied by a named parameter.
              valSupplied <- forM argsExpr $ \(argName, _) ->
                case argName of
                  Just (Symbol symbol) ->
                    case lookup (Symbol symbol) (fromMaybe [] args) of
                      Just _  -> do
                        maybeVar <- lookupVar (Symbol symbol)
                        case maybeVar of
                          Just _ -> do
                            warnC sourcePos $ "supplied parameter shadows variable from the calling scope: " ++ symbol
                            return ()
                          Nothing -> return ()
                        return (Just (Symbol symbol), True)
                      Nothing ->
                        -- FIXME: maybe is a workaround here.
                        if isJust args
                          then do
                          warnC sourcePos $ "supplied parameter not listed in module declaration: " ++ symbol
                          return (Nothing, False)
                          else return (Nothing, False)
                  Nothing -> return (Nothing, False)
              -- Find what arguments are satisfied by a default value.
              valAvailable <- forM argsExpr $ \(argName, _) ->
                case argName of
                  Just symbol  ->
                    case lookup symbol (fromMaybe [] args) of
                      Just hasDefault -> return (Just symbol, hasDefault)
                      Nothing         -> return (Nothing, False)
                  Nothing -> return (Nothing, False)
              -- Union the two sets:
              valFound <- forM argsExpr $ \(argName, _) ->
                case argName of
                  Just symbol -> do
                    let
                      supplied = fromMaybe False $ lookup (Just symbol) valSupplied
                      available = fromMaybe False $ lookup (Just symbol) valAvailable
                    return (Just symbol, supplied || available)
                  Nothing -> return (Nothing, False)
              -- Find what unnamed parameters were passed in.
              valUnnamed <- forM argsExpr $ \(argName, expr) ->
                case argName of
                  Just _  -> return Nothing
                  Nothing -> return $ Just expr
              -- ... and count them.
              let
                isSatisfied :: (Maybe Symbol, Bool) -> Bool
                isSatisfied (_, True) = True
                isSatisfied (_, False) = False
                valNamedCount = length $ filter isSatisfied valSupplied
                valFoundCount = length $ filter isSatisfied valFound
                valUnnamedCount = length $ filter isJust valUnnamed
              -- FIXME: take into account the ordering of unnamed value application.
              if valFoundCount + valUnnamedCount < length args
                then do
                errorC sourcePos $ "Insufficient parameters. found " ++ show valNamedCount ++ " named parameters, and " ++ show valUnnamedCount ++ " un-named parameters. Expected " ++ show (length args) ++ " Parameters."
                return ()
                else return ()
              -- Evaluate all of the arguments.
              argsVal <- forM argsExpr $ \(posName, expr) -> do
                val <- evalExpr sourcePos expr
                return (posName, val)
              varlookup <- getVarLookup
              -- Run the function.
              childVals <- fmap reverse $ runSuiteCapture varlookup suite
              let
                argparser = mod' childVals
                ioNewVals = fromMaybe (return []) (fst $ argMap argsVal argparser)
              liftIO ioNewVals
            Just foo            -> do
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
    return res

-- | because this runs in the IO monad, it can't backpropogate error messages.
runSuiteCaptureIO :: VarLookup -> FilePath -> ScadOpts -> [StatementI] -> IO [OVal]
runSuiteCaptureIO varlookup path opts suite = do
    (res, _) <- liftIO $ runStateT
        (runSuite suite >> getVals)
        (CompState (varlookup, [], path, [], opts))
    return res
