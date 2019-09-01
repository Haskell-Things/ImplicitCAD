-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use a shorter form of Name and Var.
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat) where

import Prelude (String, Maybe(Just, Nothing), IO, concat, ($), map, return, zip, (!!), const, (++), foldr, concatMap, (.), (<$>))

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern(ListP, Wild, Name),
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

-- Add a message to our list of messages contained in the StatE monad.
addMessage :: MessageType -> SourcePosition -> String -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text
  where
    addMesg :: Message -> StateE ()
    addMesg = modify . (\message (ExprState (a, b, messages, c)) -> ExprState (a, b, messages ++ [message], c))

-- Log an error condition.
errorE :: SourcePosition -> String -> StateE ()
errorE = addMessage Error

-- | Return the names of all of the patterns in the given pattern.
patVars :: Pattern -> [String]
patVars (Name (Symbol name)) = [name]
patVars (ListP pats) = concatMap patVars pats
patVars Wild = []

-- | Match patterns and ovals, returning a list of all of the OVals matched.
patMatch :: Pattern -> OVal -> Maybe [OVal]
patMatch (Name _) val = Just [val]
patMatch (ListP pats) (OList vals) = concat <$> zipWithM patMatch pats vals
patMatch Wild _ = Just []
patMatch _ _ = Nothing

-- | Construct a VarLookup from the given Pattern and OVal, if possible.
matchPat :: Pattern -> OVal -> Maybe VarLookup
matchPat pat val = VarLookup . fromList . zip (map Symbol $ patVars pat) <$> patMatch pat val

-- | The entry point from StateC. evaluates an expression, returning the result, and moving any error messages generated into the calling StateC.
evalExpr :: SourcePosition -> Expr -> StateC OVal
evalExpr pos expr = do
    varlookup <- getVarLookup
    (valf, ExprState (_, _, messages, _)) <- liftIO $ runStateT (evalExpr' expr) $ ExprState (varlookup, [], [], pos)
    let
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    mapM_ moveMessage messages
    return $ valf []

-- The expression evaluators.
evalExpr' :: Expr -> StateE ([OVal] -> OVal)

-- Evaluate a variable lookup.
evalExpr' (Var (Symbol name)) = do
  (ExprState (VarLookup varlookup, namestack, _, spos)) <- get
  case (lookup (Symbol name) varlookup, elemIndex name namestack) of
        (_, Just pos) -> return (!! pos)
        (Just val, _) -> return $ const val
        _             -> do
          errorE spos ("Variable " ++ name ++ " not in scope")
          return $ const OUndefined

-- Evaluate a literal value.
evalExpr' (LitE  val) = return $ const val

-- Evaluate a list of expressions.
evalExpr' (ListE exprs) = do
    valFuncs <- mapM evalExpr' exprs
    return $ \s -> OList $ ($s) <$> valFuncs

-- Evaluate application of a function.
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

-- Evaluate a lambda function.
evalExpr' (LamE pats fexpr) = do
    fparts <- forM pats $ \pat -> do
        modify (\(ExprState (a,b,c,d)) -> ExprState (a, patVars pat ++ b,c,d))
        return $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs ++ xss)
            Nothing -> OError ["Pattern match failed"]
    fval <- evalExpr' fexpr
    return $ foldr ($) fval fparts

