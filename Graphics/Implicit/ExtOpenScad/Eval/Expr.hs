-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat) where

import Prelude (String, Maybe(Just, Nothing), IO, ($), fmap, pure, zip, (!!), const, (<>), foldr, foldMap, (.), (<$>), traverse)

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

import Data.Foldable (fold, traverse_)

import Data.Traversable (for)

import Control.Monad (zipWithM)

import "monads-tf" Control.Monad.State (StateT, get, modify, liftIO, runStateT)

newtype ExprState = ExprState (VarLookup, [String], [Message], SourcePosition)
type StateE = StateT ExprState IO

-- Add a message to our list of messages contained in the StatE monad.
addMessage :: MessageType -> SourcePosition -> String -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text
  where
    addMesg :: Message -> StateE ()
    addMesg = modify . (\message (ExprState (a, b, messages, c)) -> ExprState (a, b, messages <> [message], c))

-- Log an error condition.
errorE :: SourcePosition -> String -> StateE ()
errorE = addMessage Error

-- | The names of all of the patterns in the given pattern.
patVars :: Pattern -> [String]
patVars (Name (Symbol name)) = [name]
patVars (ListP pats) = foldMap patVars pats
patVars Wild = []

-- | Match patterns and ovals, returning a list of all of the OVals matched.
patMatch :: Pattern -> OVal -> Maybe [OVal]
patMatch (Name _) val = Just [val]
patMatch (ListP pats) (OList vals) = fold <$> zipWithM patMatch pats vals
patMatch Wild _ = Just []
patMatch _ _ = Nothing

-- | Construct a VarLookup from the given Pattern and OVal, if possible.
matchPat :: Pattern -> OVal -> Maybe VarLookup
matchPat pat val = VarLookup . fromList . zip (Symbol <$> patVars pat) <$> patMatch pat val

-- | The entry point from StateC. evaluates an expression, pureing the result, and moving any error messages generated into the calling StateC.
evalExpr :: SourcePosition -> Expr -> StateC OVal
evalExpr pos expr = do
    varlookup <- getVarLookup
    (valf, ExprState (_, _, messages, _)) <- liftIO $ runStateT (evalExpr' expr) $ ExprState (varlookup, [], [], pos)
    let
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    traverse_ moveMessage messages
    pure $ valf []

-- The expression evaluators.
evalExpr' :: Expr -> StateE ([OVal] -> OVal)

-- Evaluate a variable lookup.
evalExpr' (Var (Symbol name)) = do
  (ExprState (VarLookup varlookup, namestack, _, spos)) <- get
  case (lookup (Symbol name) varlookup, elemIndex name namestack) of
        (_, Just pos) -> pure (!! pos)
        (Just val, _) -> pure $ const val
        _             -> do
          errorE spos ("Variable " <> name <> " not in scope")
          pure $ const OUndefined

-- Evaluate a literal value.
evalExpr' (LitE  val) = pure $ const val

-- Evaluate a list of expressions.
evalExpr' (ListE exprs) = do
    valFuncs <- traverse evalExpr' exprs
    pure $ \s -> OList $ ($s) <$> valFuncs

-- Evaluate application of a function.
evalExpr' (fexpr :$ argExprs) = do
    fValFunc <- evalExpr' fexpr
    argValFuncs <- traverse evalExpr' argExprs
    pure $ \s -> app (fValFunc s) (fmap ($s) argValFuncs)
        where
            app f l = case (getErrors f, getErrors $ OList l) of
                (Nothing, Nothing) -> app' f l where
                    app' (OFunc f') (x:xs) = app (f' x) xs
                    app' a [] = a
                    app' x _ = OError ["Can't apply arguments to " <> oTypeStr x]
                (Just err, _     ) -> OError [err]
                (_,      Just err) -> OError [err]

-- Evaluate a lambda function.
evalExpr' (LamE pats fexpr) = do
    fparts <- for pats $ \pat -> do
        modify (\(ExprState (a,b,c,d)) -> ExprState (a, patVars pat <> b,c,d))
        pure $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs <> xss)
            Nothing -> OError ["Pattern match failed"]
    fval <- evalExpr' fexpr
    pure $ foldr ($) fval fparts

