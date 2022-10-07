{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, rawRunExpr, matchPat, StateE, ExprState(ExprState), messages, addMessage) where

import Prelude (String, Maybe(Just, Nothing), ($), pure, zip, (!!), const, (<>), foldr, foldMap, (.), (<$>), traverse)

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

import Data.Functor.Identity (Identity)

import Data.Traversable (for)

import Control.Monad (zipWithM)

import Data.Text.Lazy (Text, unpack)

import Control.Monad.State (StateT, get, modify, runState)

data ExprState = ExprState
  { _scadVars  :: VarLookup
  , patterns  :: [String]
  , messages  :: [Message]
  , _sourcePos :: SourcePosition
  }

type StateE = StateT ExprState Identity

-- Add a message to our list of messages contained in the StatE monad.
addMessage :: MessageType -> SourcePosition -> Text -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text
  where
    addMesg :: Message -> StateE ()
    addMesg m = modify $ \s -> s { messages = messages s <> pure m }

-- Log an error condition.
errorE :: SourcePosition -> Text -> StateE ()
errorE = addMessage Error

-- | The names of all of the patterns in the given pattern.
patVars :: Pattern -> [Text]
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
    vars <- getVarLookup
    let
      (valf, s) = runState (evalExpr' expr) $ ExprState vars [] [] pos
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    traverse_ moveMessage $ messages s
    pure $ valf []

-- A more raw entry point, that does not depend on IO.
rawRunExpr :: SourcePosition -> VarLookup -> Expr -> (OVal, [Message])
rawRunExpr pos vars expr = do
  let
    (valf, s) = runState (evalExpr' expr) $ ExprState vars [] [] pos
  (valf [], messages s)

-- The expression evaluators.
evalExpr' :: Expr -> StateE ([OVal] -> OVal)

-- Evaluate a variable lookup.
evalExpr' (Var (Symbol name)) = do
  (ExprState (VarLookup varlookup) namestack _ spos) <- get
  case (lookup (Symbol name) varlookup, elemIndex (unpack name) namestack) of
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
    pure $ \s -> OList $ ($ s) <$> valFuncs

-- Evaluate application of a function.
evalExpr' (fexpr :$ argExprs) = do
    fValFunc <- evalExpr' fexpr
    argValFuncs <- traverse evalExpr' argExprs
    pure $ \s -> app (fValFunc s) (($ s) <$> argValFuncs)
        where
            app f l = case (getErrors f, getErrors $ OList l) of
                (Nothing, Nothing) -> app' f l
                    where
                        -- apply function to the list of its arguments until we run out
                        -- of them
                        app' (OFunc f') (x:xs) = app (f' x) xs
                        app' a [] = a
                        app' x _ = OError $ "Can't apply arguments to " <> oTypeStr x
                (Just err, _     ) -> OError err
                (_,      Just err) -> OError err

-- Evaluate a lambda function.
evalExpr' (LamE pats fexpr) = do
    fparts <- for pats $ \pat -> do
        modify $ \s -> s { patterns = (unpack <$> patVars pat) <> patterns s}
        pure $ \f xss -> OFunc $ \val -> case patMatch pat val of
            Just xs -> f (xs <> xss)
            Nothing -> OError "Pattern match failed"
    fval <- evalExpr' fexpr
    pure $ foldr ($) fval fparts

