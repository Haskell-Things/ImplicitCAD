{-# LANGUAGE ViewPatterns #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat) where

import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.OVal
import Graphics.Implicit.ExtOpenScad.Util.StateC

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, get, modify, liftIO)


patVars :: Pattern -> [String]
patVars (Name  name) = [name]
patVars (ListP pats) = concat $ map patVars pats
patVars _ = []

patMatch :: Pattern -> OVal -> Maybe [OVal]
patMatch (Name _) val = Just [val]
patMatch (ListP pats) (OList vals) = do
    matches <- Monad.zipWithM patMatch pats vals
    return $ concat matches
patMatch Wild _ = Just []
patMatch _ _ = Nothing

matchPat :: Pattern -> OVal -> Maybe VarLookup
matchPat pat val = do
    let vars = patVars pat
    vals <- patMatch pat val
    return $ Map.fromList $ zip vars vals


evalExpr :: Expr -> StateC OVal
evalExpr expr = do
    varlookup  <- getVarLookup
    (valf, _) <- liftIO $ State.runStateT (evalExpr' expr) (varlookup, [])
    return $ valf []



evalExpr' :: Expr -> StateT (VarLookup, [String]) IO ([OVal] -> OVal)

evalExpr' (Var   name ) = do
    (varlookup, namestack) <- get
    return $
        case (Map.lookup name varlookup, List.findIndex (==name) namestack) of
            (_, Just pos) -> \s -> s !! pos
            (Just val, _) -> const val
            _             -> const $ OError ["Variable " ++ name ++ " not in scope" ]   

evalExpr' (LitE  val  ) = return $ const val

evalExpr' (ListE exprs) = do
    valFuncs <- Monad.mapM evalExpr' exprs
    return $ \s -> OList $ map ($s) valFuncs

evalExpr' (fexpr :$ argExprs) = do
    fValFunc <- evalExpr' fexpr
    argValFuncs <- Monad.mapM evalExpr' argExprs
    return $ \s -> app (fValFunc s) (map ($s) argValFuncs)
        where 
            app f l = case (getErrors f, getErrors $ OList l) of
                (Nothing, Nothing) -> app' f l where
                    app' (OFunc f) (x:xs) = app (f x) xs
                    app' a [] = a
                    app' x _ = OError ["Can't apply arguments to " ++ oTypeStr x]
                (Just err, _     ) -> OError [err]
                (_,      Just err) -> OError [err]

evalExpr' (LamE pats fexpr) = do
    fparts <- Monad.forM pats $ \pat -> do
        modify (\(vl, names) -> (vl, patVars pat ++ names))
        return $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs ++ xss)
            Nothing -> OError ["Pattern match failed"]
    fval <- evalExpr' fexpr
    return $ foldr ($) fval fparts


--------------

{-
simplifyExpr ((simplifyExpr -> Var f) :$ args) = (Var f :$) $
    let
        split b l = (filter b l, filter (not.b) l)
        args' = map simplifyExpr args
        (numArgs, nonNumArgs) = split (\x -> case x of LitE (ONum n) -> True; _ -> False) args'
        numArgs' = map (\(LitE (ONum n)) -> n) numArgs
    in case f of
        "+" -> (LitE $ ONum $ sum  numArgs'):nonNumArgs
        "*" -> (LitE $ ONum $ product numArgs'):nonNumArgs
        _ -> args'
simplifyExpr x = x
-}
