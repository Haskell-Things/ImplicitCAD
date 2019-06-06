-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat) where

import Prelude (String, Maybe(Just, Nothing), IO, concat, ($), map, return, zip, (!!), const, (++), foldr, concatMap)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern(Name, ListP, Wild),
                                                  OVal(OList, OError, OFunc),
                                                  Expr(LitE, Var, ListE, LamE, (:$)),
                                                  VarLookup)

import Graphics.Implicit.ExtOpenScad.Util.OVal (oTypeStr, getErrors)
import Graphics.Implicit.ExtOpenScad.Util.StateC (StateC, getVarLookup)

import Data.List (elemIndex)
import Data.Map (fromList, lookup)
import Control.Monad (zipWithM, mapM, forM)
import Control.Monad.State (StateT, get, modify, liftIO, runStateT)

import Control.Arrow (second)

patVars :: Pattern -> [String]
patVars (Name  name) = [name]
patVars (ListP pats) = concatMap patVars pats
patVars _ = []

patMatch :: Pattern -> OVal -> Maybe [OVal]
patMatch (Name _) val = Just [val]
patMatch (ListP pats) (OList vals) = do
    matches <- zipWithM patMatch pats vals
    return $ concat matches
patMatch Wild _ = Just []
patMatch _ _ = Nothing

matchPat :: Pattern -> OVal -> Maybe VarLookup
matchPat pat val = do
    let vars = patVars pat
    vals <- patMatch pat val
    return $ fromList $ zip vars vals

evalExpr :: Expr -> StateC OVal
evalExpr expr = do
    varlookup  <- getVarLookup
    (valf, _) <- liftIO $ runStateT (evalExpr' expr) (varlookup, [])
    return $ valf []

evalExpr' :: Expr -> StateT (VarLookup, [String]) IO ([OVal] -> OVal)

evalExpr' (Var   name ) = do
    (varlookup, namestack) <- get
    return $
        case (lookup name varlookup, elemIndex name namestack) of
            (_, Just pos) -> (!! pos)
            (Just val, _) -> const val
            _             -> const $ OError ["Variable " ++ name ++ " not in scope" ]

evalExpr' (LitE  val  ) = return $ const val

evalExpr' (ListE exprs) = do
    valFuncs <- mapM evalExpr' exprs
    return $ \s -> OList $ map ($s) valFuncs

evalExpr' (fexpr :$ argExprs) = do
    fValFunc <- evalExpr' fexpr
    argValFuncs <- mapM evalExpr' argExprs
    return $ \s -> app (fValFunc s) (map ($s) argValFuncs)
        where
            app f l = case (getErrors f, getErrors $ OList l) of
                (Nothing, Nothing) -> app' f l where
                    app' (OFunc f') (x:xs) = app (f' x) xs
                    app' a [] = a
                    app' x _ = OError ["Can't apply arguments to " ++ oTypeStr x]
                (Just err, _     ) -> OError [err]
                (_,      Just err) -> OError [err]

evalExpr' (LamE pats fexpr) = do
    fparts <- forM pats $ \pat -> do
        modify (second (patVars pat ++))
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
