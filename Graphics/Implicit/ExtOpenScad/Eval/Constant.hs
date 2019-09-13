-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants, runExpr) where

import Prelude (String, Maybe(Just, Nothing), IO, ($), return, (+), Either (Left, Right), Char, Bool(False))

import Graphics.Implicit.Definitions (Fastℕ)

import Graphics.Implicit.ExtOpenScad.Definitions (
                                                  Pattern,
                                                  Expr,
                                                  VarLookup,
                                                  Message,
                                                  MessageType(SyntaxError),
                                                  StateC,
                                                  ScadOpts(ScadOpts),
                                                  CompState(CompState),
                                                  SourcePosition(SourcePosition),
                                                  OVal(OUndefined),
                                                  varUnion
                                                 )

import Graphics.Implicit.ExtOpenScad.Util.StateC (modifyVarLookup, addMessage)

import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat)

import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)

import Control.Monad.State (liftIO, runStateT)

import System.Directory (getCurrentDirectory)

import Text.Parsec (SourceName, parse, ParseError)

import Text.Parsec.String (GenParser)

import Text.Parsec.Error (errorMessages, showErrorMessages)

import Graphics.Implicit.ExtOpenScad.Parser.Util (patternMatcher)

import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchTok)

-- | Define variables used during the extOpenScad run.
addConstants :: [String] -> IO (VarLookup, [Message])
addConstants constants = do
  path <- getCurrentDirectory
  let scadOpts = ScadOpts False False
  (_, CompState (varLookup, _, _, messages, _)) <- liftIO $ runStateT (execAssignments constants 0) (CompState (defaultObjects, [], path, [], scadOpts))
  return (varLookup, messages)
  where
    execAssignments :: [String] -> Fastℕ  -> StateC ()
    execAssignments [] _ = return ()
    execAssignments (assignment:xs) count = do
      let
        pos = (SourcePosition count 1 "cmdline_constants")
        show' err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
      case parseAssignment "cmdline_constant" assignment of
        Left e -> do
          addMessage SyntaxError pos $ show' e
        Right (key, expr) -> do
          res <- evalExpr pos expr
          case matchPat key res of
            Nothing -> return ()
            Just pat -> modifyVarLookup $ varUnion pat
      execAssignments xs (count+1)
    parseAssignment :: SourceName -> String -> Either ParseError (Pattern, Expr)
    parseAssignment = parse assignment
      where
        assignment :: GenParser Char st (Pattern, Expr)
        assignment = do
          key <- patternMatcher
          _ <- matchTok '='
          expr <- expr0
          return (key, expr)

-- | Evaluate an expression, returning only it's result.
runExpr :: String -> IO (OVal, [Message])
runExpr expression = do
  path <- getCurrentDirectory
  let scadOpts = ScadOpts False False
  (res, CompState (_, _, _, messages, _)) <- liftIO $ runStateT (execExpression expression) (CompState (defaultObjects, [], path, [], scadOpts))
  return (res, messages)
  where
    execExpression :: String -> StateC OVal
    execExpression expr = do
      let
        pos = (SourcePosition 1 1 "raw_expression")
        show' err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
      case parseExpression "raw_expression" expr of
        Left e -> do
          addMessage SyntaxError pos $ show' e
          return OUndefined
        Right parseRes -> evalExpr pos parseRes
    parseExpression :: SourceName -> String -> Either ParseError Expr
    parseExpression = parse expr0

