-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

-- Allow us to treat incomplete tuples as function references.
{-# LANGUAGE TupleSections #-}

module Graphics.Implicit.ExtOpenScad.Eval.Expr (evalArgs, evalExpr, rawRunExpr, matchPat, StateE, ExprState(ExprState), addMessage) where

import Prelude (String, Monoid, Maybe(Just, Nothing), Bool (False, True), ($), elem, mempty, pure, show, zip, (&&), const, (<>), foldr, foldMap, (.), (<$>), traverse)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern(Name, ListP, Wild),
                                                  OVal(OList, OError, OFunc, OUndefined, OUModule, ONModule, ONModuleWithSuite, OVargsModule),
                                                  Expr(LitE, ListE, LamE, Var, (:$)),
                                                  Symbol(Symbol),
                                                  VarLookup(VarLookup),
                                                  SourcePosition,
                                                  Message(Message),
                                                  MessageType(Error),
                                                  StateC, ImplicitCadM, runImplicitCadM
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.ArgParser (argMap)

import Graphics.Implicit.ExtOpenScad.Util.OVal (oTypeStr, getErrors)

import Graphics.Implicit.ExtOpenScad.Util.StateC (errorC, getVarLookup)

import qualified Graphics.Implicit.ExtOpenScad.Util.StateC as GIEUS (addMessage)

import Graphics.Implicit.ExtOpenScad.Eval.Module (checkOptions, runModule)

import Data.Maybe (fromMaybe, isNothing)

import Data.Map (fromList, lookup)

import Data.Foldable (fold, traverse_)

import Data.Traversable (for)

import Control.Monad (unless, zipWithM)

import Data.Text.Lazy (Text, pack, unpack)

import Data.Eq (Eq, (==))
import Text.Show (Show)
import Control.Monad.Writer.Class (tell)
import Control.Monad.State.Lazy (get)
import Control.Monad.State.Class (modify)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ask)

-- Patterns is the only thing being modified, so
-- it is the only on in the state structure.
newtype ExprState = ExprState
  { patterns  :: [String]
  } deriving (Eq, Show)

-- varLookup and sourcePos are only ever read from
-- so we can put them into a reader, so they can never
-- accidentally be written to.
data Input = Input
  { _varLookup :: VarLookup
  , _sourcePos :: SourcePosition
  } deriving (Eq, Show)

-- Check Graphics.Implicit.ExtOpenScad.Definitions for an explanation
-- of why we are using a transformer stack.
type StateE a = ImplicitCadM Input [Message] ExprState Identity a

runStateE :: Input -> ExprState -> StateE a -> (a, [Message], ExprState)
runStateE r s m = runIdentity $ runImplicitCadM r s m

-- | Add a message to our list of messages contained in the StateE monad.
addMessage :: MessageType -> SourcePosition -> Text -> StateE ()
addMessage mtype pos text = addMesg $ Message mtype pos text
  where
    addMesg :: Message -> StateE ()
    addMesg = tell . pure

-- | Log an error condition.
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

-- | Evaluate the arguments, turning them from expressions into values.
evalArgs :: [(Maybe Symbol, Expr)] -> SourcePosition -> StateC [(Maybe Symbol, OVal)]
evalArgs args sourcePos = for args $ \(posName, expr) -> do
  val <- evalExpr sourcePos expr
  pure (posName, val)

-- | The entry point from StateC. Evaluates either an expression or an eligible module call.
evalExpr :: SourcePosition -> Expr -> StateC OVal
evalExpr sourcePos expr = case expr of
                            (maybeMod :$ argExprs) -> do
                              -- Yes, we're recursing, after dropping argument expressions, for the OVal
                              rVal <- evalExpr sourcePos maybeMod
                              if isModule rVal
                                then do
                                -- Perform a module call.
                                res <- runExprModule sourcePos rVal argExprs
                                pure $ canonicalizeRes $ OList res
                                else
                                -- Evaluate expression.
                                evalExprStateC sourcePos expr
                            _ -> evalExprStateC sourcePos expr
  where
    isModule (OUModule {}) = True
    isModule (ONModule {}) = True
    isModule (ONModuleWithSuite {}) = True
    isModule (OVargsModule {}) = True
    isModule _ = False
    -- FIXME: We may need a better result cannonicalizer here.
    canonicalizeRes (OList [oneItem]) = oneItem
    canonicalizeRes other = other

-- | Execute a module call, in place of an expression.
runExprModule :: SourcePosition -> OVal -> [Expr] -> StateC [OVal]
runExprModule sourcePos mod argExprsRaw = do
  let
    -- Mark all of our arguments as unnamed. There are no named arguments in expressions.
    argExprs = (Nothing,) <$> argExprsRaw
    -- Common error messages.
    noSuiteError,notModError :: (Monoid a) => StateC a
    noSuiteError = do
      errorC sourcePos $ "tried to use a " <> oTypeStr mod <> " that uses suites on the right hand side of assignment."
      pure mempty
    notModError = do
      errorC sourcePos $ "tried to run something that is not a module:" <> pack (show mod)
      pure mempty

  -- Fully evaluate arguments. Since we're in Expr context, we can only handle unnamed arguments.
  evaluatedArgs <- evalArgs argExprs sourcePos

  -- We can't handle any suites, either.
  _ <- case mod of
         (OUModule {}) -> pure mempty :: StateC ()
         (ONModule {}) -> pure mempty
         (ONModuleWithSuite {}) -> noSuiteError
         (OVargsModule {}) -> noSuiteError
         _ -> notModError

  -- Perform any per-module-type specific housework, and call the module.
  case mod of
    (OUModule (Symbol name) args implementation) -> do
      -- User modules can only have one instance, so we only have to check one set of options here.
      optionsMatch <- checkOptions args argExprs True sourcePos
      unless optionsMatch (errorC sourcePos $ "Options check failed when executing user-defined module " <> name <> ".")
      varLookup <- getVarLookup
      -- Run the module.
      runModule sourcePos $ argMap evaluatedArgs $ implementation varLookup
    (ONModule _ implementation _) -> do
      -- Run the module.
      runModule sourcePos $ argMap evaluatedArgs $ implementation sourcePos
    (ONModuleWithSuite {}) -> noSuiteError
    (OVargsModule {}) -> noSuiteError
    _ -> notModError

-- | The inner monadic entry point. Evaluates an expression, pureing the result, and moving any error messages generated into the calling StateC.
evalExprStateC :: SourcePosition -> Expr -> StateC OVal
evalExprStateC pos expr = do
    vars <- getVarLookup
    let
      input = Input vars pos
      initState = ExprState []
      (valf, messages, _) = runStateE input initState (evalExpr' expr)
      moveMessage (Message mtype mpos text) = GIEUS.addMessage mtype mpos text
    traverse_ moveMessage messages
    pure $ valf []

-- A pure entry point, that does not do module calls, and does not depend on IO.
rawRunExpr :: SourcePosition -> VarLookup -> Expr -> (OVal, [Message])
rawRunExpr pos vars expr = do
  let
    input = Input vars pos
    initState = ExprState []
    (valf, messages, _) = runStateE input initState (evalExpr' expr)
  (valf [], messages)

-- The expression evaluators.
evalExpr' :: Expr -> StateE ([OVal] -> OVal)

-- Evaluate a variable lookup.
evalExpr' (Var (Symbol name)) = do
  Input (VarLookup varlookup) spos <- ask
  (ExprState namestack) <- get
  let v = lookup (Symbol name) varlookup
      n = unpack name `elem` namestack
  case (v, n) of
    (_, True) -> pure $ \l ->
      let m = foldr
            -- Scan for variable names from the end of the list (newest), and also
            -- ensure that we aren't overriding values if we have already found one.
            -- All in all, this should ensure that we aren't seeing the off by 1 error
            -- when looking up the values for function parameters as raised in this issue.
            -- https://github.com/Haskell-Things/ImplicitCAD/issues/431
            (\(n', v') z -> if isNothing z && unpack name == n' then pure v' else z)
            Nothing $
            -- Zip the names and incoming values so that when looking up values
            -- we are ensuring that names are paired with values. When a LamE is evaled
            -- it is possible that a name is pushed and then used before a value is pushed
            -- and this zip neatly handles that situation.
            zip namestack l
      in fromMaybe OUndefined m
    (Just o, _) -> pure $ const o
    _ -> do
      errorE spos ("Variable " <> name <> "not in scope")
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
                        -- Apply a function to the list of its arguments until we run out
                        -- of them
                        app' (OFunc f') (x:xs) = app (f' x) xs
                        app' a [] = a
                        app' x _ = OError $ "Can't apply arguments to " <> oTypeStr x
                (Just err, _     ) -> OError err
                (_,      Just err) -> OError err

-- Evaluate a lambda function.
evalExpr' (LamE pats fexpr) = do
    fparts <- for pats $ \pat -> do
        -- Add new names to the end of the list so that names and values aren't
        -- effectively shifted by 1 when a name is defined but the value hasn't been
        -- calculated yet. This also allows us to neatly zip names and values ensuring
        -- we are only looking at names with defined values.
        modify $ \s -> s { patterns = patterns s <> (unpack <$> patVars pat)}
        pure $ \f xss -> OFunc $ \val -> case patMatch pat val of
            -- Push values to the end once they are calculated.
            Just xs -> f (xss <> xs)
            Nothing -> OError "Pattern match failed"
    fval <- evalExpr' fexpr
    pure $ foldr ($) fval fparts
