-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why is this required?
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI) where

import Prelude(Maybe(Just, Nothing), Bool(True, False), Either(Left, Right), (.), ($), show, return, (++), reverse, fst, snd, readFile, filter, length, (<), (>), (&&), (==), (/=), map, fmap, flip, elem, not, zip, init, last)

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
import Graphics.Implicit.ExtOpenScad.Util.StateC (errorC, warnC, modifyVarLookup, mapMaybeM, scadOptions, lookupVar, pushVals, getRelPath, withPathShiftedBy, getVals, putVals, addMessage, getVarLookup)
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Data.List (intercalate)

import Data.Map (union, fromList, toList)

import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)

import Control.Monad (forM_, forM, mapM_, when)

import Control.Monad.State (get, liftIO, runStateT, (>>))

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
          forM_ (toList match) $ \(Symbol varName, _) -> do
            maybeVar <- lookupVar (Symbol varName)
            when (isJust maybeVar)
              (warnC sourcePos $ "redefining already defined object: " ++ show varName)
            modifyVarLookup $ varUnion (VarLookup match)
        (_,   Nothing ) -> errorC sourcePos "pattern match failed in assignment"

-- | Interpret an if conditional statement.
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
              optionsMatch <- checkOptions args True
              when (optionsMatch == False) (errorC sourcePos $ "Options check failed when executing user-defined module " ++ name)
              evaluatedArgs <- evalArgs argsExpr
              -- Evaluate the suite.
              suiteResults <- runSuiteCapture varlookup suite
              when (suite /= []) (errorC sourcePos $ "Suite provided, but module " ++ name ++ " does not accept one. Perhaps a missing semicolon?")
              -- Run the module.
              let
                argsMapped = argMap evaluatedArgs $ mod' suiteResults
              forM_ (snd argsMapped) $ errorC sourcePos
              fromMaybe (return []) (fst argsMapped)
            Just (ONModule _ implementation forms) -> do
              possibleInstances <- selectInstances forms
              let
                suiteInfo = case possibleInstances of
                  [(_, suiteInfoFound)]   -> suiteInfoFound
                  []                      -> Nothing
                  ((_, suiteInfoFound):_) -> suiteInfoFound
              when (length possibleInstances < 1) (do
                                                      errorC sourcePos $ "no instance of " ++ name ++ " found to match given parameters.\nInstances available:\n" ++ show (ONModule (Symbol name) implementation forms)
                                                      mapM_ (flip checkOptions True) $ fmap (Just . fst) forms)
{-              when (length possibleInstances > 1) (do
                                                      errorC sourcePos $ "too many instances of " ++ name ++ " have been found that match given parameters."
                                                      mapM_ (flip checkOptions True) $ fmap (Just . fst) possibleInstances) -}
              -- Evaluate all of the arguments.
              evaluatedArgs <- evalArgs argsExpr
              -- Evaluate the suite.
              vals <- runSuiteCapture varlookup suite
              suiteResults <- case suiteInfo of
                              Just True -> do
                                when (vals == []) (errorC sourcePos "Suite required, but none provided.")
                                return vals
                              Just False -> return vals
                              _ -> do
                                when (suite /= []) (errorC sourcePos $ "Suite provided, but module " ++ name ++ " does not accept one. Perhaps a missing semicolon?")
                                return []
              -- Run the module.
              let
                argsMapped = argMap evaluatedArgs $ implementation sourcePos suiteResults
              forM_ (snd argsMapped) $ errorC sourcePos
              fromMaybe (return []) (fst argsMapped)
            Just (OVargsModule modname mod') -> do
              -- Evaluate all of the arguments.
              evaluatedArgs <- evalArgs argsExpr
              -- Run the module, which evaluates it's own suite.
              _ <- mod' modname sourcePos evaluatedArgs suite runSuite -- no values are returned
              return []
            Just foo -> do
                    case getErrors foo of
                        Just err -> errorC sourcePos err
                        Nothing  -> errorC sourcePos $ "Object " ++ name ++ " is not a module!"
                    return []
            _ -> do
                errorC sourcePos $ "Module " ++ name ++ " not in scope."
                return []
        pushVals newVals
          where
            selectInstances :: [([(Symbol, Bool)], Maybe Bool)] -> StateC ([([(Symbol, Bool)], Maybe Bool)])
            selectInstances instances = do
              validInstances <- forM instances $
                    ( \(args, suiteInfo) -> do
                        res <- checkOptions (Just args) False
                        return $ case res of
                                   True -> (Just (args, suiteInfo))
                                   _    -> Nothing
                    )
              return $ catMaybes validInstances
            checkOptions :: (Maybe [(Symbol, Bool)]) -> Bool -> StateC (Bool)
            checkOptions args makeWarnings = do
              -- Find what arguments are satisfied by a named parameter.
              valNamedFound <- namedValuesSatisfied $ fromMaybe [] args
              let
                -- Find what arguments are satisfied by a default value, were given in a named parameter, or were given.. and count them.
                valDefaulted  = map fst $ filter (\t -> snd t == True) $ fromMaybe [] args
                valNotDefaulted = map fst $ filter (\t -> snd t == False) $ fromMaybe [] args
                valUnnamed = unnamedParameters argsExpr
                valUnnamedCount = length $ valUnnamed
                valNamedCount = length $ namedParameters argsExpr
                mappedDefaulted = filter (\t -> t `elem` valNamedFound) valDefaulted
                mappedNotDefaulted = filter (\t -> t `elem` valNamedFound) valNotDefaulted
                notMappedDefaultable = filter (\t -> not (t `elem` mappedDefaulted)) valDefaulted
                notMappedNotDefaultable = filter (\t -> not (t `elem` mappedNotDefaulted)) valNotDefaulted
                mapableFromUnnamed = filter ( \t -> (fst t) `elem` (notMappedDefaultable ++ notMappedNotDefaultable)) $ fromMaybe [] args
                mapFromUnnamed = zip mapableFromUnnamed valUnnamed
--                mappedDefaultable = filter (\t -> fst (fst t) `elem` valDefaulted) mapFromUnnamed
--                mappedNotDefaultable = filter (\t -> fst (fst t) `elem` valNotDefaulted) mapFromUnnamed 
                missingNotDefaultable = filter (\t -> not (fst (fst t) `elem` valDefaulted)) mapFromUnnamed
--                extraUnnamed = filter (\t -> not (t `elem` (valDefaulted ++ valNotDefaulted) ++ (map fst $ map fst $ mappedNotDefaultable ++ mappedDefaultable))) $ namedParameters argsExpr
                extraUnnamed = filter (\t -> not (t `elem` (valDefaulted ++ valNotDefaulted))) $ namedParameters argsExpr
                parameterReport =  "Passed " ++
                  (if valNamedCount > 0 then (show valNamedCount ++ (if valNamedCount == 1 then " named parameter" else " named parameters")) else "" ) ++
                  (if valNamedCount > 0 && valUnnamedCount > 0 then (", and ") else "") ++
                  (if valUnnamedCount > 0 then (show valUnnamedCount ++ (if valUnnamedCount == 1 then " un-named parameter." else " un-named parameters.")) else ".") ++
                  (if length missingNotDefaultable > 0 then
                      (if (length missingNotDefaultable) == 1
                       then " Couldn't match one parameter: " ++ (show $ fst $ fst $ last missingNotDefaultable)
                       else " Couldn't match " ++ show missingNotDefaultable ++ " parameters: " ++ (intercalate ", " $ map show $ map fst $ init missingNotDefaultable) ++ " and " ++ (show $ fst $ last missingNotDefaultable) ++ "."
                      ) else "") ++
                  (if length extraUnnamed > 0 then
                      (if (length extraUnnamed) == 1
                       then " Had one extra parameter: " ++ (show $ last extraUnnamed) 
                       else " Had " ++ (show $ length extraUnnamed) ++ " extra parameters. They are:" ++ (intercalate ", " $ map show $ init extraUnnamed) ++ " and " ++ (show $ last extraUnnamed) ++ "."
                      ) else "")
{-                  ++
                  (if (length mappedDefaultable > 0 || length mappedNotDefaultable > 0) then (
                      "Mapped " ++
                      (if length mappedDefaultable > 0 then
                         (if (length mappedDefaultable == 1) then "one defaultable un-named parameter" else (show (length mappedDefaultable)) ++ " defaultable un-named parameters") else "") ++
                      (if length mappedDefaultable > 0 && length mappedNotDefaultable > 0 then "and " else "") ++
                      (if length mappedNotDefaultable > 0 then
                         (if (length mappedNotDefaultable == 1) then "one required un-named parameter." else (show (length mappedDefaultable)) ++ " required un-named parameters.") else "")) else "") ++
                  (if length mapableFromUnnamed > 0 then
                      (if (length mapableFromUnnamed) == 1
                       then " Considered one un-named parameter mapable: " ++ (show $ fst $ last mapableFromUnnamed) 
                       else " Considered " ++ show (length mapableFromUnnamed) ++ " parameters mapable: " ++ (intercalate ", " $ map show $ map fst $ init mapableFromUnnamed) ++ " and " ++ (show $ fst $ last mapableFromUnnamed) ++ "."
                      ) else "") -}
              when ((length missingNotDefaultable) > 0 && makeWarnings)
                (errorC sourcePos $ "Insufficient parameters. " ++ parameterReport)
              when (length extraUnnamed > 0 && isJust args && makeWarnings)
                (errorC sourcePos $ "Too many parameters: " ++ (show $ length extraUnnamed) ++ " extra. " ++ parameterReport)
              return $ (length missingNotDefaultable) == 0 && (length extraUnnamed) == 0
            namedValuesSatisfied :: [(Symbol, Bool)] -> StateC ([Symbol])
            namedValuesSatisfied args = fmap catMaybes $ forM args $ 
              \(symbol, _) -> do
                maybeVar <- lookupVar symbol
                return $ if isJust maybeVar
                         then Nothing
                         else Just symbol
            namedParameters :: [(Maybe Symbol, Expr)] -> [Symbol]
            namedParameters args = mapMaybe (
              \(argName, _) -> argName
              ) $ args
            unnamedParameters :: [(Maybe Symbol, Expr)] -> [Expr]
            unnamedParameters args = mapMaybe (
              \(argName, expr) ->
                case argName of
                  Just _  -> Nothing
                  Nothing -> Just expr
              ) $ args
            evalArgs :: [(Maybe Symbol, Expr)] -> StateC([(Maybe Symbol, OVal)])
            evalArgs args = forM args $ \(posName, expr) -> do
              val <- evalExpr sourcePos expr
              return (posName, val)

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
