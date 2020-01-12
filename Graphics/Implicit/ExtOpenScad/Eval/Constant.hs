-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

module Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants, runExpr) where

import Prelude (String, IO, ($), pure, (+), Either,  Bool(False), (.), either, (<$>), (<*), (<*>), (<$))

import Data.Foldable (traverse_, foldlM)

import Graphics.Implicit.Definitions (Fastℕ)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern,
                                                  Expr,
                                                  VarLookup,
                                                  Message,
                                                  MessageType(SyntaxError),
                                                  StateC,
                                                  ScadOpts(ScadOpts),
                                                  CompState(CompState, scadVars, messages),
                                                  SourcePosition(SourcePosition),
                                                  OVal(OUndefined),
                                                  varUnion
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.StateC (modifyVarLookup, addMessage)

import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)

import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)

import "monads-tf" Control.Monad.State (liftIO, runStateT, (>>=))

import System.Directory (getCurrentDirectory)

import Text.Parsec (SourceName, parse, ParseError)

import Text.Parsec.Error (errorMessages, showErrorMessages)

import Graphics.Implicit.ExtOpenScad.Parser.Util (patternMatcher)

import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchTok)

-- | Define variables used during the extOpenScad run.
addConstants :: [String] -> IO (VarLookup, [Message])
addConstants constants = do
  path <- getCurrentDirectory
  (_, s) <- liftIO . runStateT (execAssignments constants) $ CompState defaultObjects [] path [] opts
  pure (scadVars s, messages s)
  where
    opts = ScadOpts False False
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    execAssignments :: [String] -> StateC Fastℕ
    execAssignments = foldlM execAssignment 0
    execAssignment :: Fastℕ -> String -> StateC Fastℕ
    execAssignment count assignment = do
      let pos = SourcePosition count 1 "cmdline_constants"
          err = addMessage SyntaxError pos . show'
          run (k, e) = evalExpr pos e >>= traverse_ (modifyVarLookup . varUnion) . matchPat k
      either err run $ parseAssignment "cmdline_constant" assignment
      pure $ count + 1
    parseAssignment :: SourceName -> String -> Either ParseError (Pattern, Expr)
    parseAssignment = parse $ (,) <$> patternMatcher <* matchTok '=' <*> expr0

-- | Evaluate an expression, pureing only it's result.
-- FIXME: improve the expression evaluator so that we can remove the IO in this.
runExpr :: String -> IO (OVal, [Message])
runExpr expression = do
  (res, s) <- initState <$> getCurrentDirectory >>= runStateT (execExpression expression)
  pure (res, messages s)
  where
    initState path = CompState defaultObjects [] path [] $ ScadOpts False False
    initPos = SourcePosition 1 1 "raw_expression"
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    oUndefined e = OUndefined <$ addMessage SyntaxError initPos (show' e)
    execExpression :: String -> StateC OVal
    execExpression = either oUndefined (evalExpr initPos) . parse expr0 "raw_expression"

