-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Allow us to use a shorter form of Name.
{-# LANGUAGE PatternSynonyms #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram) where

import Prelude(Char, Either, String, return, fmap, ($), (>>), Bool(False, True), map)

import Data.Maybe(Maybe(Just, Nothing))

import Data.Functor.Identity(Identity)

-- We use parsec to parse.
import Text.ParserCombinators.Parsec (SourceName, try, sepBy, GenParser, oneOf, space, char, getPosition, parse, many1, eof, string, ParseError, many, noneOf, (<|>), (<?>))
import Text.Parsec.Prim (ParsecT)

import Graphics.Implicit.ExtOpenScad.Definitions (Statement(DoNothing, NewModule, Include, Echo, If, For, ModuleCall,(:=)),Expr(LamE), StatementI(StatementI), Symbol(Symbol), SourcePosition)

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util (genSpace, tryMany, stringGS, (*<|>), (?:), patternMatcher, variableSymb, sourcePosition)

-- the top level of the expression parser.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- Let us use the old syntax when defining Names.
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram name s = parse program name s where
    program :: ParsecT String u Identity [StatementI]
    program = do
        sts <- many1 computation
        eof
        return sts

-- | A computable block of code in our openscad-like programming language.
computation :: GenParser Char st StatementI
computation =
    do -- suite statements: no semicolon...
        _ <- genSpace
        s <- tryMany [
            ifStatementI,
            forStatementI,
            throwAway,
            userModuleDeclaration
            ]
        _ <- genSpace
        return s
    *<|> do -- Non suite statements. Semicolon needed...
        _ <- genSpace
        s <- tryMany [
            echo,
            include, -- also handles use
            function,
            assignment
            ]
        _ <- stringGS " ; "
        return s
    *<|> do -- Modules. no semicolon...
        _ <- genSpace
        s <- userModule
        _ <- genSpace
        return s

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
suite = (fmap return computation <|> do
    _ <- char '{'
    _ <- genSpace
    stmts <- many (try computation)
    _ <- genSpace
    _ <- char '}'
    return stmts
    ) <?> " suite"

-- | commenting out a comuptation: use % or * before the statement, and it will not be run.
throwAway :: GenParser Char st StatementI
throwAway = do
    pos <- sourcePos
    _ <- genSpace
    _ <- oneOf "%*"
    _ <- genSpace
    _ <- computation
    return $ StatementI pos DoNothing

-- | An include! Basically, inject another extopenscad file here...
include :: GenParser Char st StatementI
include = (do
    pos <- sourcePos
    injectVals <-  (string "include" >> return True )
               <|> (string "use"     >> return False)
    _ <- stringGS " < "
    filename <- many (noneOf "<> ")
    _ <- stringGS " > "
    return $ StatementI pos $ Include filename injectVals
    ) <?> "include "

-- | An assignment (parser)
assignment :: GenParser Char st StatementI
assignment = ("assignment " ?:) $ do
    pos <- sourcePos
    lvalue <- patternMatcher
    _ <- stringGS " = "
    valExpr <- expr0
    return $ StatementI pos $ lvalue := valExpr

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = ("function " ?:) $ do
    pos <- sourcePos
    varSymb <- string "function" >> space >> genSpace >> variableSymb
    _ <- stringGS " ( "
    argVars <- sepBy patternMatcher (stringGS " , ")
    _ <- stringGS " ) = "
    valExpr <- expr0
    return $ StatementI pos $ Name varSymb := LamE argVars valExpr

-- | An echo (parser)
echo :: GenParser Char st StatementI
echo = do
    pos <- sourcePos
    _ <- stringGS "echo ( "
    exprs <- expr0 `sepBy` stringGS " , "
    _ <- stringGS " ) "
    return $ StatementI pos $ Echo exprs

ifStatementI :: GenParser Char st StatementI
ifStatementI = "if " ?: do
    pos <- sourcePos
    _ <- stringGS "if ( "
    bexpr <- expr0
    _ <- stringGS " ) "
    sTrueCase <- suite
    _ <- genSpace
    sFalseCase <- (stringGS "else " >> suite ) *<|> return []
    return $ StatementI pos $ If bexpr sTrueCase sFalseCase

forStatementI :: GenParser Char st StatementI
forStatementI = "for " ?: do
    pos <- sourcePos
    -- a for loop is of the form:
    --      for ( vsymb = vexpr   ) loops
    -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
    -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
    _ <- stringGS "for ( "
    lvalue <- patternMatcher
    _ <- stringGS " = "
    vexpr <- expr0
    _ <- stringGS " ) "
    loopContent <- suite
    return $ StatementI pos $ For lvalue vexpr loopContent

-- | parse a call to a module.
userModule :: GenParser Char st StatementI
userModule = do
    pos <- sourcePos
    name <- variableSymb
    _ <- genSpace
    args <- moduleArgsUnit
    _ <- genSpace
    s <- suite *<|> (stringGS " ; " >> return [])
    return $ StatementI pos $ ModuleCall (Symbol name) args s

-- | declare a module.
userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = do
    pos <- sourcePos
    _ <- stringGS "module "
    newModuleName <- variableSymb
    _ <- genSpace
    args <- moduleArgsUnitDecl
    _ <- genSpace
    s <- suite
    return $ StatementI pos $ NewModule (Symbol newModuleName) args s

-- | parse the arguments passed to a module.
moduleArgsUnit :: GenParser Char st [(Maybe Symbol, Expr)]
moduleArgsUnit = do
    _ <- stringGS " ( "
    args <- sepBy (
        do
            -- eg. a = 12
            symb <- variableSymb
            _ <- stringGS " = "
            expr <- expr0
            return (Just (Symbol symb), expr)
        *<|> do
            -- eg. a(x,y) = 12
            symb <- variableSymb
            _ <- stringGS " ( "
            argVars <- sepBy variableSymb (try $ stringGS " , ")
            _ <- stringGS " ) = "
            expr <- expr0
            return (Just (Symbol symb), LamE (map Name argVars) expr)
        *<|> do
            -- eg. 12
            expr <- expr0
            return (Nothing, expr)
        ) (try $ stringGS " , ")
    _ <- stringGS " ) "
    return args

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl ::  GenParser Char st [(Symbol, Maybe Expr)]
moduleArgsUnitDecl = do
    _ <- stringGS " ( "
    argTemplate <- sepBy (
        do
            symb <- variableSymb;
            _ <- stringGS " = "
            expr <- expr0
            return (Symbol symb, Just expr)
        *<|> do
            symb <- variableSymb;
            _ <- stringGS " ( "
                 -- FIXME: why match this content, then drop it?
            _ <- sepBy variableSymb (try $ stringGS " , ")
            _ <- stringGS " ) = "
            expr <- expr0
            return (Symbol symb, Just expr)
        *<|> do
            symb <- variableSymb
            return (Symbol symb, Nothing)
        ) (try $ stringGS " , ")
    _ <- stringGS " ) "
    return argTemplate

-- | Find the source position. Used when generating errors.
sourcePos :: ParsecT s u Identity SourcePosition
sourcePos = do
  pos <- getPosition
  return $ sourcePosition pos
