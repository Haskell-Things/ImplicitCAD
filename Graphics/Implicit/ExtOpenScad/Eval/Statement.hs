{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.OVal
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Graphics.Implicit.ExtOpenScad.Util.StateC
import Graphics.Implicit.ExtOpenScad.Eval.Expr


import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import           Control.Monad.State (State,StateT, get, put, modify, liftIO)


runStatementI :: StatementI -> StateC ()

runStatementI (StatementI lineN (pat := expr)) = do
	val <- evalExpr expr
	let posMatch = matchPat pat val
	case (getErrors val, posMatch) of
		(Just err,  _ ) -> errorC lineN err
		(_, Just match) -> modifyVarLookup $ Map.union match
		(_,   Nothing ) -> errorC lineN "pattern match failed in assignment"

runStatementI (StatementI lineN (Echo exprs)) = do
	let
		show2 (OString s) = s
		show2 x = show x
	vals <- mapM evalExpr exprs
	case getErrors (OList vals) of
		Nothing  -> liftIO $ putStrLn $ concat $ map show2 vals
		Just err -> errorC lineN err

runStatementI (StatementI lineN (For pat expr loopContent)) = do
	val <- evalExpr expr
	case (getErrors val, val) of
		(Just err, _)      -> errorC lineN err
		(_, OList vals) -> Monad.forM_ vals $ \v ->
			case matchPat pat v of
				Just match -> do
					modifyVarLookup $ Map.union match
					runSuite loopContent
				Nothing -> return ()
		_ -> return ()

runStatementI (StatementI lineN (If expr a b)) = do
	val <- evalExpr expr
	case (getErrors val, val) of
		(Just err,  _  )  -> errorC lineN ("In conditional expression of if statement: " ++ err)
		(_, OBool True )  -> runSuite a
		(_, OBool False)  -> runSuite b
		_                 -> return ()

runStatementI (StatementI lineN (NewModule name argTemplate suite)) = do
	argTemplate' <- Monad.forM argTemplate $ \(name, defexpr) -> do
		defval <- mapMaybeM evalExpr defexpr 
		return (name, defval)
	varlookup <- getVarLookup
	runStatementI $ StatementI lineN $ (Name name :=) $ LitE $ OModule $ \vals -> do 
		newNameVals <- Monad.forM argTemplate' $ \(name, maybeDef) -> do
			val <- case maybeDef of
				Just def -> argument name `defaultTo` def
				Nothing  -> argument name
			return (name, val)
		let
			children = ONum $ fromIntegral $ length vals
			child = OModule $ \vals -> do
				n :: ℕ <- argument "n";
				return $ return $ return $ 
					if n <= length vals
						then vals !! n
						else OUndefined
			childBox = OFunc $ \n -> case fromOObj n :: Maybe ℕ of
				Just n  | n < length vals -> case vals !! n of
					-- _ -> toOObj $ getBox3 obj3
					-- _ -> toOObj $ getBox2 obj2
					_ -> OUndefined
				_ -> OUndefined
			newNameVals' = newNameVals ++ [("children", children),("child", child), ("childBox", childBox)]
			varlookup' = Map.union (Map.fromList newNameVals) varlookup
			suiteVals  = runSuiteCapture varlookup' suite
		return suiteVals

runStatementI (StatementI lineN (ModuleCall name argsExpr suite)) = do
		maybeMod  <- lookupVar name
		varlookup <- getVarLookup
		childVals <- liftIO $ runSuiteCapture varlookup suite
		argsVal   <- Monad.forM argsExpr $ \(posName, expr) -> do
			val <- evalExpr expr
			return (posName, val)
		newVals <- case maybeMod of
			Just (OModule mod) -> liftIO ioNewVals  where
				argparser = mod childVals
				ioNewVals = case fst $ argMap argsVal argparser of
					Just iovals -> iovals
					Nothing     -> return []
			Just foo            -> do
					case getErrors foo of
						Just err -> errorC lineN err
						Nothing  -> errorC lineN $ "Object called not module!"
					return []
			Nothing -> do
				errorC lineN $ "Module " ++ name ++ " not in scope."
				return []
		pushVals newVals



runSuite :: [StatementI] -> StateC ()
runSuite stmts = Monad.mapM_ runStatementI stmts

runSuiteCapture :: VarLookup -> [StatementI] -> IO [OVal]
runSuiteCapture varlookup suite =
	fmap fst $ State.runStateT suiteExec (varlookup, [])
		where suiteExec = do 
				runSuite suite
				(_, vals) <- get
				return vals



