-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use a shorter form of Name.
{-# LANGUAGE PatternSynonyms #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram) where

import Prelude(Char, Either, String, ($), (*>), Bool(False, True), (<$>), (<*>), (.), (<$), flip, fmap, filter, not, pure)

import Data.Maybe(Maybe(Just, Nothing))

import Graphics.Implicit.ExtOpenScad.Definitions (Statement(DoNothing, NewModule, Include, If, ModuleCall, (:=)), Expr(LamE), StatementI(StatementI), Symbol(Symbol), SourcePosition)

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), patternMatcher, sourcePosition)

-- the top level of the expression parser.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchFunction, matchInclude, matchUse, matchIf, matchElse, matchModule, matchTok, matchComma, matchSemi, surroundedBy, matchIdentifier)

-- We use parsec to parse.
import Text.Parsec (SourceName, (<?>), sepBy, oneOf, getPosition, parse, eof, ParseError, many, noneOf, option, between, char, optionMaybe)

import Text.Parsec.String (GenParser)

import Control.Applicative ((<*), (<|>))

import Data.Functor (($>))

-- Let us use the old syntax when defining Names.
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

data CompIdx = A1 | A2

-- | all of the token parsers are lexemes which consume all trailing spaces nicely.
-- | This leaves us to deal only with the first spaces in the file.
parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program where
    program :: GenParser Char st [StatementI]
    program = removeNoOps <$> (whiteSpace *> many (computation A1) <* eof)


-- | A computable block of code in our openscad-like programming language.
computation :: CompIdx -> GenParser Char st StatementI
computation A1 =
  computation A2
  <|>
  throwAway

computation A2 =
  -- suite statements: no semicolon...
  userModule
  <|>
  ifStatementI
  <|>
  userModuleDeclaration
  <|> -- Non suite statements. Semicolon needed...
  ( include
    <|>
    function
  ) <* matchSemi
  *<|>
  assignment <* matchSemi

-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of computables which
--  are in turn StatementI s.
suite :: GenParser Char st [StatementI]
suite = (
    removeNoOps . (:[]) <$> computation A1
  *<|>
    removeNoOps <$> surroundedBy '{' (many (computation A1)) '}'
  ) <?> "suite"

-- | Every StatementI requires a source position, thus we can build a combinator.
statementI :: GenParser Char st (Statement StatementI) -> GenParser Char st StatementI
statementI p = StatementI <$> sourcePos <*> p

-- | Commenting out a computation: use % or * before the statement, and it will not be run.
throwAway :: GenParser Char st StatementI
throwAway = statementI $ DoNothing <$ oneOf "%*" <* whiteSpace <* computation A2

-- | An include! Basically, inject another extopenscad file here...
include :: GenParser Char st StatementI
include = statementI p <?> "include/use"
  where
    p :: GenParser Char st (Statement StatementI)
    p = flip Include
      <$> (matchInclude $> True <|> matchUse $> False)
      -- FIXME: better definition of valid filename characters.
      <*> between (char '<') (matchTok '>') (many $ noneOf "<> ")

-- | An assignment (parser)
assignment :: GenParser Char st StatementI
assignment = statementI p <?> "assignment"
  where
    p :: GenParser Char st (Statement StatementI)
    p = (:=) <$> patternMatcher <* matchTok '=' <*> expr0

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = statementI p <?> "function"
  where
    p :: GenParser Char st (Statement StatementI)
    p = (:=) <$> lval <*> rval
    lval :: GenParser Char st GIED.Pattern
    lval = Name <$> (matchFunction *> matchIdentifier)
    rval :: GenParser Char st Expr
    rval = LamE <$> surroundedBy '(' (sepBy patternMatcher matchComma) ')' <*> (matchTok '=' *> expr0)

-- | An if statement (parser)
ifStatementI :: GenParser Char st StatementI
ifStatementI = statementI p <?> "if"
  where
    p :: GenParser Char st (Statement StatementI)
    p = If <$> (matchIf *> surroundedBy '(' expr0 ')') <*> suite <*> option [] (matchElse *> suite)

-- | parse a call to a module.
userModule :: GenParser Char st StatementI
userModule = statementI p <?> "module call"
  where
    p :: GenParser Char st (Statement StatementI)
    p = ModuleCall <$> fmap Symbol matchIdentifier <*> moduleArgsUnit <*> (suite *<|> (matchSemi $> []))

-- | declare a module.
userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = statementI p <?> "module declaration"
  where
    p :: GenParser Char st (Statement StatementI)
    p = NewModule <$> fmap Symbol (matchModule *> matchIdentifier) <*> moduleArgsUnitDecl <*> suite

-- | parse the arguments passed to a module.
moduleArgsUnit :: GenParser Char st [(Maybe Symbol, Expr)]
moduleArgsUnit =
    surroundedBy '('
      (sepBy (
        do
            -- eg. a = 12
            symb <- matchIdentifier
            expr <- matchTok '=' *> expr0
            pure (Just (Symbol symb), expr)
        *<|> do
            -- eg. a(x,y) = 12
            symb <- matchIdentifier
            argVars <- surroundedBy '(' (sepBy matchIdentifier matchComma) ')'
            expr <- matchTok '=' *> expr0
            pure (Just (Symbol symb), LamE (fmap Name argVars) expr)
        *<|> do
            -- eg. 12
            expr <- expr0
            pure (Nothing, expr)
      ) matchComma)
      ')'

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl ::  GenParser Char st [(Symbol, Maybe Expr)]
moduleArgsUnitDecl =
    surroundedBy '('
      (sepBy (
        do
          symb <- matchIdentifier
          expr <- optionMaybe (matchTok '=' *> expr0)
          pure (Symbol symb, expr)
      ) matchComma)
      ')'

-- | Find the source position. Used when generating errors.
sourcePos :: GenParser Char st SourcePosition
sourcePos = sourcePosition <$> getPosition

isNoOp :: StatementI -> Bool
isNoOp (StatementI _ DoNothing) = True
isNoOp _                        = False

-- | Remove statements that do nothing.
removeNoOps :: [StatementI] -> [StatementI]
removeNoOps = filter $ not . isNoOp
