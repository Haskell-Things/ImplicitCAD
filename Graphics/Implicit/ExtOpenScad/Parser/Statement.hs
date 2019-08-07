-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use a shorter form of Name.
{-# LANGUAGE PatternSynonyms #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram) where

import Prelude(Char, Either, String, return, fmap, ($), (>>), Bool(False, True), map, (<$>), (.))

import Data.Maybe(Maybe(Just, Nothing))

-- We use parsec to parse.
import Text.Parsec (SourceName, (<|>), (<?>), try, sepBy, oneOf, char, getPosition, parse, eof, ParseError, many, noneOf, option)
import Text.Parsec.String (GenParser)

import Graphics.Implicit.ExtOpenScad.Definitions (Statement(DoNothing, NewModule, Include, Echo, If, For, ModuleCall,(:=)),Expr(LamE), StatementI(StatementI), Symbol(Symbol), SourcePosition)

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util (tryMany, (*<|>), (?:), patternMatcher, sourcePosition)

-- the top level of the expression parser.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchFunction, matchInclude, matchUse, matchEcho, matchIf, matchElse, matchFor, matchModule, matchTok, matchComma, surroundedBy, matchIdentifier)

-- Let us use the old syntax when defining Names.
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

data CompIdx = A1 | A2

parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program where
    program :: GenParser Char st [StatementI]
    program = do
         -- all of the token parsers are lexemes which consume all trailing spaces nicely.
         -- This leaves us to deal only with the first spaces in the file.
        sts <- whiteSpace >> many (computation A1)
        _   <- eof
        return (removeNoOps sts)

-- | A computable block of code in our openscad-like programming language.
computation :: CompIdx -> GenParser Char st StatementI
computation A1 =
  computation A2
  *<|>
  throwAway

computation A2 =
    -- suite statements: no semicolon...
        tryMany [
            ifStatementI,
            forStatementI,
            userModuleDeclaration
            ]
    *<|> do -- Non suite statements. Semicolon needed...
        s <- tryMany [
            echo,
            include, -- also handles use
            function,
            assignment
            ]
        _ <- matchTok ';'
        return s
    *<|> -- Modules. no semicolon...
        userModule

{-
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
-}
suite :: GenParser Char st [StatementI]
suite = (
  fmap return (computation A1)
  <|>
    do
      stmts <- surroundedBy '{' (many (computation A1)) '}'
      return (removeNoOps stmts)
  ) <?> " suite"

-- | commenting out a computation: use % or * before the statement, and it will not be run.
throwAway :: GenParser Char st StatementI
throwAway = do
    pos <- sourcePos
    _ <- oneOf "%*"
    _ <- whiteSpace
    _ <- computation A2
    return $ StatementI pos DoNothing

-- | An include! Basically, inject another extopenscad file here...
include :: GenParser Char st StatementI
include = ("include " ?:) $ do
    pos <- sourcePos
    injectVals <-  (matchInclude >> return True )
               <|> (matchUse     >> return False)
    _ <- char '<'
    -- FIXME: better definition of valid filename characters.
    filename <- many (noneOf "<> ")
    _ <- matchTok '>'
    return $ StatementI pos $ Include filename injectVals

-- | An assignment (parser)
assignment :: GenParser Char st StatementI
assignment = ("assignment " ?:) $ do
    pos <- sourcePos
    lvalue <- patternMatcher
    _ <- matchTok '='
    valExpr <- expr0
    return $ StatementI pos $ lvalue := valExpr

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = ("function " ?:) $ do
    pos <- sourcePos
    _ <- matchFunction
    varSymb <- matchIdentifier
    argVars <- surroundedBy '(' (sepBy patternMatcher (try matchComma)) ')'
    _ <- matchTok '='
    valExpr <- expr0
    return $ StatementI pos $ Name varSymb := LamE argVars valExpr

-- | An echo (parser)
echo :: GenParser Char st StatementI
echo = ("echo " ?:) $ do
    pos <- sourcePos
    _ <- matchEcho
    exprs <- surroundedBy '(' (sepBy expr0 (try matchComma)) ')'
    return $ StatementI pos $ Echo exprs

ifStatementI :: GenParser Char st StatementI
ifStatementI = "if " ?: do
    pos <- sourcePos
    _ <- matchIf
    bexpr <- surroundedBy '(' expr0 ')'
    sTrueCase <- suite
    sFalseCase <- option [] (matchElse >> suite)
    return $ StatementI pos $ If bexpr sTrueCase sFalseCase

forStatementI :: GenParser Char st StatementI
forStatementI = "for " ?: do
    pos <- sourcePos
    -- a for loop is of the form:
    --      for ( vsymb = vexpr   ) loops
    -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
    -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
    _ <- matchFor
    _ <- matchTok '('
    lvalue <- patternMatcher
    _ <- matchTok '='
    vexpr <- expr0
    _ <- matchTok ')'
    StatementI pos . For lvalue vexpr <$> suite

-- | parse a call to a module.
userModule :: GenParser Char st StatementI
userModule = do
    pos <- sourcePos
    name <- matchIdentifier
    args <- moduleArgsUnit
    s <- suite *<|> (matchTok ';' >> return [])
    return $ StatementI pos $ ModuleCall (Symbol name) args s

-- | declare a module.
userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = do
    pos <- sourcePos
    _ <- matchModule
    newModuleName <- matchIdentifier
    args <- moduleArgsUnitDecl
    StatementI pos . NewModule (Symbol newModuleName) args <$> suite

-- | parse the arguments passed to a module.
moduleArgsUnit :: GenParser Char st [(Maybe Symbol, Expr)]
moduleArgsUnit = do
    _ <- matchTok '('
    args <- sepBy (
        do
            -- eg. a = 12
            symb <- matchIdentifier
            _ <- matchTok '='
            expr <- expr0
            return (Just (Symbol symb), expr)
        *<|> do
            -- eg. a(x,y) = 12
            symb <- matchIdentifier
            argVars <- surroundedBy '(' (sepBy matchIdentifier (try matchComma)) ')'
            _ <- matchTok '='
            expr <- expr0
            return (Just (Symbol symb), LamE (map Name argVars) expr)
        *<|> do
            -- eg. 12
            expr <- expr0
            return (Nothing, expr)
        ) (try matchComma)
    _ <- matchTok ')'
    return args

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl ::  GenParser Char st [(Symbol, Maybe Expr)]
moduleArgsUnitDecl = do
    _ <- matchTok '('
    argTemplate <- sepBy (
        do
            symb <- matchIdentifier
            _ <- matchTok '='
            expr <- expr0
            return (Symbol symb, Just expr)
        *<|> do
            symb <- matchIdentifier
            _ <- matchTok '('
                 -- FIXME: why match this content, then drop it?
            _ <- sepBy matchIdentifier (try matchComma)
            _ <- matchTok ')'
            _ <- matchTok '='
            expr <- expr0
            return (Symbol symb, Just expr)
        *<|> do
            symb <- matchIdentifier
            return (Symbol symb, Nothing)
        ) (try matchComma)
    _ <- matchTok ')'
    return argTemplate

-- | Find the source position. Used when generating errors.
sourcePos :: GenParser Char st SourcePosition
sourcePos = sourcePosition <$> getPosition

-- | Remove statements that do nothing.
removeNoOps :: [StatementI] -> [StatementI]
removeNoOps [] = []
removeNoOps (StatementI _ DoNothing:sts) = removeNoOps sts
removeNoOps (st:sts) = st : removeNoOps sts

