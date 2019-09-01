-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants) where

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
                                                  varUnion,
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

-- | to define variables used during the extOpenScad run.
addConstants :: [String] -> IO (VarLookup, [Message])
addConstants constants = do
  path <- getCurrentDirectory
  let scadOpts = ScadOpts False False False
  (_, CompState (varLookup, _, _, messages, _)) <- liftIO $ runStateT (execAssignments constants 0) (CompState (defaultObjects, [], path, [], scadOpts))
  return (varLookup, messages)
  where
    execAssignments :: [String] -> Fastℕ  -> StateC ()
    execAssignments [] _ = return ()
    execAssignments (assignment:xs) count = do
      let
        pos = (SourcePosition count 1 "cmdline_constants")
        show' err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
      case parseExpr "cmdline_constant" assignment of
        Left e -> do
          addMessage SyntaxError pos $ show' e 
          return ()
        Right (key, expr) -> do
          res <- evalExpr pos expr
          case matchPat key res of
            Nothing -> return ()
            Just pat -> modifyVarLookup $ varUnion pat
          return ()
      execAssignments xs (count+1)
    parseExpr :: SourceName -> String -> Either ParseError (Pattern, Expr)
    parseExpr = parse assignment
      where
        assignment :: GenParser Char st (Pattern, Expr)
        assignment = do
          key <- patternMatcher
          _ <- matchTok '='
          expr <- expr0
          return (key, expr)
