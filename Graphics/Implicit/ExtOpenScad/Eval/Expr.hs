-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat) where

import Prelude (String, Maybe(Just, Nothing), IO, concat, ($), map, return, zip, (!!), const, (++), foldr, concatMap, (.))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern(Name, ListP, Wild),
                                                  OVal(OList, OError, OFunc, OUndefined),
                                                  Expr(LitE, ListE, LamE, Var, (:$)),
                                                  Symbol(Symbol),
                                                  VarLookup(VarLookup),
                                                  SourcePosition,
                                                  Message(Message),
                                                  MessageType(Error),
                                                  StateC
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.OVal (oTypeStr, getErrors)

import Graphics.Implicit.ExtOpenScad.Util.StateC (getVarLookup)

import qualified Graphics.Implicit.ExtOpenScad.Util.StateC as GIEUS (addMessage)

import Data.List (elemIndex)

import Data.Map (fromList, lookup)

import Control.Monad (zipWithM, mapM, forM, mapM_)

import Control.Monad.State (StateT, get, modify, liftIO, runStateT)

newtype ExprState = ExprState (VarLookup, [String], [Message], SourcePosition)
type StateE = StateT ExprState IO

addMesg :: Message -> StateE ()
addMesg = modify . (\message (ExprState (a, b, messages, c)) -> ExprState (a, b, messages ++ [message], c))

addMessage :: MessageType -> SourcePosition -> String -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text

errorE :: SourcePosition -> String -> StateE ()
errorE = addMessage Error

--epos :: ExprState -> SourcePosition
--epos (ExprState (_, _, _, pos)) = pos

patVars :: Pattern -> [String]
patVars (Name  (Symbol name)) = [name]
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
    let vars = map Symbol $ patVars pat
    vals <- patMatch pat val
    return $ VarLookup $ fromList $ zip vars vals

evalExpr :: SourcePosition -> Expr -> StateC OVal
evalExpr pos expr = do
    varlookup <- getVarLookup
    (valf, ExprState (_, _, messages, _)) <- liftIO $ runStateT (evalExpr' expr) $ ExprState (varlookup, [], [], pos)
    let
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    mapM_ moveMessage messages
    return $ valf []

evalExpr' :: Expr -> StateE ([OVal] -> OVal)

evalExpr' (Var (Symbol name)) = do
  (ExprState (VarLookup varlookup, namestack, _, spos)) <- get
  case (lookup (Symbol name) varlookup, elemIndex name namestack) of
        (_, Just pos) -> return (!! pos)
        (Just val, _) -> return $ const val
        _             -> do
          errorE spos ("Variable " ++ name ++ " not in scope")
          return $ const OUndefined

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
        modify (\(ExprState (a,b,c,d)) -> ExprState (a, patVars pat ++ b,c,d))
        return $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs ++ xss)
            Nothing -> OError ["Pattern match failed"]
    fval <- evalExpr' fexpr
    return $ foldr ($) fval fparts

{-
evalExpr' _ = do
  state <- get
  errorE (epos state) "Fallthrough in expression parser."
  return $ const OUndefined 
-}

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
