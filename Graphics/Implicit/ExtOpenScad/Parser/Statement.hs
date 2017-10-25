-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

module Graphics.Implicit.ExtOpenScad.Parser.Statement where

import Prelude(Char, Either, String, Maybe(Just, Nothing), Monad, return, fmap, ($), (>>), Bool(False, True), map)

import Text.ParserCombinators.Parsec (Column, GenParser, Line, SourceName, ParseError, char, eof, getPosition, many, many1, noneOf, optional, oneOf, parse, sepBy, sourceColumn, sourceLine, space, string, try, (<|>))

import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity(Identity)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Name), Statement(DoNothing, NewModule, Include, Echo, If, For, ModuleCall,(:=)),Expr(LamE), StatementI(StatementI))
import Graphics.Implicit.ExtOpenScad.Parser.Util (genSpace, tryMany, stringGS, (*<|>), (?:), patternMatcher, variableSymb)
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

parseProgram :: SourceName -> [Char] -> Either ParseError [StatementI]
parseProgram name s = parse program name s where
  program :: ParsecT [Char] u Identity [StatementI]
  program = do
    sts <- many1 computation
    eof
    return sts

-- | A in our programming openscad-like programming language.
computation :: GenParser Char st StatementI
computation =
  do -- suite statements: no semicolon...
    _ <- genSpace
    s <- tryMany [
      ifStatementI,
      forStatementI,
      throwAway,
      userModuleDeclaration{-,
      unimplemented "mirror",
      unimplemented "multmatrix",
      unimplemented "color",
      unimplemented "render",
      unimplemented "surface",
      unimplemented "projection",
      unimplemented "import_stl"-}
      -- rotateExtrude
      ]
    _ <- genSpace
    return s
  *<|> do -- Non suite s. Semicolon needed...
    _ <- genSpace
    s <- tryMany [
      echo,
      include,
      function,
      assignment--,
      --use
      ]
    _ <- stringGS " ; "
    return s
  *<|> do -- Modules
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
--  We consider it to be a list of s which
--  are in turn StatementI s.
--  So this parses them.
-}
suite :: GenParser Char st [StatementI]
suite = "suite" ?: (
  fmap return computation
  <|> do
      _ <- char '{'
      _ <- genSpace
      stmts <- many (try computation)
      _ <- genSpace
      _ <- char '}'
      return stmts
  )



columnNumber :: Monad m => ParsecT s u m Column
columnNumber = fmap sourceColumn getPosition

withLineNumber :: Monad m => ParsecT s u m (Statement StatementI) -> ParsecT s u m StatementI
withLineNumber e = do
  line <- lineNumber
  v <- e
  return $ StatementI line v
  where
    lineNumber :: Monad m => ParsecT s u m Line
    lineNumber = fmap sourceLine getPosition

throwAway :: GenParser Char st StatementI
throwAway = withLineNumber $ do
  _ <- genSpace
  _ <- oneOf "%*"
  _ <- genSpace
  _ <- computation
  return $ DoNothing

-- An included ! Basically, inject another openscad file here...
include :: GenParser Char st StatementI
include = "include " ?: (withLineNumber $ do
  injectVals <- (string "include" >> return True )
                 <|> (string "use" >> return False)
  _ <- stringGS " < "
  filename <- many (noneOf "<> ")
  _ <- stringGS " > "
  return $ Include filename injectVals)

-- | An assignment  (parser)
assignment :: GenParser Char st StatementI
assignment = "assignment " ?: (withLineNumber $ do
  pattern <- patternMatcher
  _ <- stringGS " = "
  valExpr <- expr0
  return $ pattern := valExpr)

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = "function " ?: (withLineNumber $ do
  varSymb <- ((optional $ string "function" >> space) >> genSpace >> variableSymb)
  _ <- stringGS " ( "
  argVars <- sepBy patternMatcher (stringGS " , ")
  _ <- stringGS " ) = "
  valExpr <- expr0
  return $ Name varSymb := LamE argVars valExpr)

-- | An echo  (parser)
echo :: GenParser Char st StatementI
echo = "echo " ?: (withLineNumber $ do
  _ <- stringGS "echo ( "
  exprs <- expr0 `sepBy` (stringGS " , ")
  _ <- stringGS " ) "
  return $ Echo exprs)

ifStatementI :: GenParser Char st StatementI
ifStatementI = "if " ?: (withLineNumber $ do
  _ <- stringGS "if ( "
  bexpr <- expr0
  _ <- stringGS " ) "
  sTrueCase <- suite
  _ <- genSpace
  sFalseCase <- (stringGS "else " >> suite ) *<|> (return [])
  return $ If bexpr sTrueCase sFalseCase)

forStatementI :: GenParser Char st StatementI
forStatementI = "for " ?: (withLineNumber $ do
  -- a for loop is of the form:
  --      for ( vsymb = vexpr   ) loops
  -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
  -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
  _ <- stringGS "for ( "
  pattern <- patternMatcher
  _ <- stringGS " = "
  vexpr <- expr0
  _ <- stringGS " ) "
  loopContent <- suite
  return $ For pattern vexpr loopContent)

userModule :: GenParser Char st StatementI
userModule = "user module " ?: (withLineNumber $ do
  name <- variableSymb
  _ <- genSpace
  args <- moduleArgsUnit
  _ <- genSpace
  s <- suite *<|> (stringGS " ; " >> return [])
  return $ ModuleCall name args s)

userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = "user module decl " ?: (withLineNumber $ do
  _ <- stringGS "module "
  newModuleName <- variableSymb
  _ <- genSpace
  args <- moduleArgsUnitDecl
  _ <- genSpace
  s <- suite
  return $ NewModule newModuleName args s)

----------------------

moduleArgsUnit :: GenParser Char st [(Maybe String, Expr)]
moduleArgsUnit = "module args " ?: do
  _ <- stringGS " ( "
  args <- sepBy (
    do
      -- eg. a = 12
      symb <- variableSymb
      _ <- stringGS " = "
      expr <- expr0
      return $ (Just symb, expr)
    *<|> do
      -- eg. a(x,y) = 12
      symb <- variableSymb
      _ <- stringGS " ( "
      argVars <- sepBy variableSymb (try $ stringGS " , ")
      _ <- stringGS " ) = "
      expr <- expr0
      return $ (Just symb, LamE (map Name argVars) expr)
    *<|> do
      -- eg. 12
      expr <- expr0
      return (Nothing, expr)
    ) (try $ stringGS " , ")
  _ <- stringGS " ) "
  return args

moduleArgsUnitDecl ::  GenParser Char st [(String, Maybe Expr)]
moduleArgsUnitDecl = do
    _ <- stringGS " ( "
    argTemplate <- sepBy (
        do
            symb <- variableSymb;
            _ <- stringGS " = "
            expr <- expr0
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb;
            _ <- stringGS " ( "
                 -- FIXME: why match this content, then drop it?
            _ <- sepBy variableSymb (try $ stringGS " , ")
            _ <- stringGS " ) = "
            expr <- expr0
-- FIXME: this line looks right, but.. what does this change?
--            return $ (Just symb, LamE (map Name argVars) expr)
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb
            return (symb, Nothing)
        ) (try $ stringGS " , ")
    _ <- stringGS " ) "
    return argTemplate

