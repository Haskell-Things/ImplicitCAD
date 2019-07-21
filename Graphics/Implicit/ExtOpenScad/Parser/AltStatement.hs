-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Enable explicit-forall syntax.
{-# LANGUAGE ExplicitForAll #-}

module Graphics.Implicit.ExtOpenScad.Parser.AltStatement (parseProgram) where

import Prelude(Char, String, Either, ($), (++), return, (<$>), (>>))

import Graphics.Implicit.ExtOpenScad.Definitions (StatementI(StatementI), Expr(LamE), Statement(Sequence, ModuleCall, NewModule, NewFunction, If, DoNothing, (:=)), Symbol(Symbol), Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchLet, matchModule, matchFunction, matchIf, matchElse, matchFor, matchTok, matchIdentifier, surroundedBy, matchSemi, matchEach)

import Graphics.Implicit.ExtOpenScad.Parser.AltExpr (expr0)

import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)

import Data.Maybe (Maybe(Just, Nothing))

import Text.Parsec (SourceName, ParseError, (<|>), getPosition, skipMany1, sepBy, try, sepEndBy, many, optionMaybe, eof, parse, option)

import Text.Parsec.String (GenParser)

import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

-- In general, the grammar is predictive and requires no more than one character of lookahead,
-- except when differentiating between keywords and identifiers.
-- Or at least, that is the goal. exceptions are documented.

parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram name s = parse program name s where
    program :: ParsecT String st Identity [StatementI]
    program = do
         -- all of the token parsers are lexemes which consume all trailing spaces nicely.
         -- This leaves us to deal only with the first spaces in the file.
        _    <- whiteSpace
        prog <- many statement
        _    <- eof
        return prog

statements :: GenParser Char st [StatementI]
statements = removeNoOps <$> many statement

statement :: GenParser Char st StatementI
statement =
        do
        _ <- matchSemi
        returnStatement DoNothing
    <|> do
        stmts <- surroundedBy '{' statements '}'
        returnStatement $ Sequence stmts
    <|> -- things starting with an identifier
        moduleInstantiationOrAssignment
    <|> -- module instantiations with '!', '#', '%', '*' prefixes
        flaggedModuleInstantiation
    <|> do -- user module declaration, e.g. "module foo(a, b = [1,2]) { }
        _          <- matchModule
        moduleName <- matchIdentifier
        argDecls   <- surroundedBy '(' moduleParametersDeclaration ')'
        stmt       <- statements
        returnStatement $ NewModule (Symbol moduleName) argDecls stmt
    <|> do -- user function declaration, e.g. "function foo(a, b = [1,2]) a;
        _          <- matchFunction
        funcName   <- matchIdentifier
        argDecls   <- surroundedBy '(' argumentsDeclaration ')'
        _          <- matchTok '='
        funcExpr   <- expression
        _          <- matchSemi
        returnStatement $ NewFunction (Symbol funcName) argDecls funcExpr
    <|>
        ifStatement

ifStatement :: ParsecT String st Identity StatementI
ifStatement = do
    _          <- matchIf
    condition  <- surroundedBy '(' expression ')'
    trueScope  <- statements
    falseScope <- option [] $ matchElse >> statements
    returnStatement $ If condition trueScope falseScope

-- Assignment and module instantiation both start with the same token, an identifier.
-- So, in order to keep the parser predictive (for performance reasons),
-- the grammar is factored to first recognize the identifier, and choose between
-- assignment and module instantiation based on the following token.
-- The module modification flags, '!', '#', '%', '*', will need to be handled in a separate
-- parser.
moduleInstantiationOrAssignment :: GenParser Char st StatementI
moduleInstantiationOrAssignment = do
        ident <- matchIdentifier
        moduleInstantiationTail ident <|> assignment (Symbol ident)
    <|> do
        _ <- matchFor
        moduleInstantiationTail "for"
    <|> do
        _ <- matchEach
        moduleInstantiationTail "each"
    <|> do
        _ <- matchLet
        moduleInstantiationTail "let"

-- An assignment statement, not a default value assignment in formal parameters or an argument assignment in calling a module.
assignment :: Symbol -> GenParser Char st StatementI
assignment ident =
    do
    _     <- matchTok '='
    expr  <- expression
    _     <- matchSemi
    returnStatement $ Name ident := expr

moduleFlag :: forall st b. Char -> ParsecT String st Identity b -> ParsecT String st Identity b
moduleFlag tok inst = matchTok tok >> inst

flaggedModuleInstantiation :: GenParser Char st StatementI
flaggedModuleInstantiation = do
        _ <- moduleFlag '!' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '#' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '%' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '*' maybeFlaggedModuleInstantiation
        returnDoNothing


maybeFlaggedModuleInstantiation :: GenParser Char st StatementI
maybeFlaggedModuleInstantiation = do
        _ <- moduleFlag '!' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '#' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '%' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        _ <- moduleFlag '*' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleName <- matchIdentifier
        moduleInstantiationTail moduleName
    <|> do
        _ <- matchFor
        moduleInstantiationTail "for"
    <|> do
        _ <- matchEach
        moduleInstantiationTail "each"
    <|> do
        _ <- matchLet
        moduleInstantiationTail "let"
    <|> do
        _ <- ifStatement
        returnDoNothing

moduleInstantiationTail :: String -> GenParser Char st StatementI
moduleInstantiationTail moduleName = do
    arguments  <- surroundedBy '(' moduleCallArguments ')'
    child      <- childStatement
    returnStatement $ ModuleCall (Symbol moduleName) arguments $ removeNoOps [child]

moduleParametersDeclaration :: GenParser Char st [(Symbol, Maybe Expr)]
moduleParametersDeclaration = sepEndBy moduleParameterDeclaration oneOrMoreCommas

moduleParameterDeclaration :: GenParser Char st (Symbol, Maybe Expr)
moduleParameterDeclaration = do
    parameterName <- matchIdentifier
    defaultExpr   <- optionMaybe defaultExpression
    return ((Symbol parameterName), defaultExpr)
    where
        defaultExpression :: ParsecT String u Identity Expr
        defaultExpression =
              lambdaDeclaration
          <|> (matchTok '=' >> expression)

argumentsDeclaration :: GenParser Char st [(Symbol, Maybe Expr)]
argumentsDeclaration = sepEndBy argumentDeclaration oneOrMoreCommas

argumentDeclaration :: GenParser Char st (Symbol, Maybe Expr)
argumentDeclaration = do
    parameterName <- matchIdentifier
    defaultExpr   <- optionMaybe defaultValueExpression
    return ((Symbol parameterName), defaultExpr)
    where
        defaultValueExpression :: ParsecT String st Identity Expr
        defaultValueExpression = matchTok '=' >> expression

childStatements :: GenParser Char st [StatementI]
childStatements = removeNoOps <$> many innerChildStatement

childStatement :: GenParser Char st StatementI
childStatement =
        do
        _ <- matchTok ';'
        returnDoNothing
    <|> do
        subStatements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence subStatements
    <|>
        maybeFlaggedModuleInstantiation

innerChildStatement :: GenParser Char st StatementI
innerChildStatement =
        do
        _ <- matchTok ';'
        returnDoNothing
    <|> do
        subStatements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence subStatements
    <|>
        moduleInstantiationOrAssignment
    <|> -- module instantiations with '!', '#', '%', '*' prefixes
        flaggedModuleInstantiation

moduleCallArguments :: GenParser Char st [(Maybe Symbol, Expr)]
moduleCallArguments = sepEndBy moduleCallArgument oneOrMoreCommas

moduleCallArgument :: GenParser Char st (Maybe Symbol, Expr)
moduleCallArgument =
    try (do
        -- In order to support the Extopenscad syntax of defining a function in the module call, we will use 'try'.
        -- The worst case performance penalty will be paid when the argument is not a lambda, but looks like one,
        -- except for the missing '= expression' part. In this case the whole attempt to parse a lambda expression
        -- argument will be rolled back and parsed a second time as an expression.
        -- considering not supporting this exact syntax if favour of one that is more manageable with an LL1 grammar.
        -- it is less costly for the 'paramName = expression' syntax if it is a miss, as only one token will be
        -- rolled back.
        paramName <- matchIdentifier
        do
            _        <- matchTok '='
            argValue <- expression
            return (Just (Symbol paramName), argValue)
         <|> do
            lambda <- lambdaDeclaration
            return (Just (Symbol paramName), lambda))
    <|> do
        argValue  <- expression
        return (Nothing, argValue)

lambdaDeclaration :: GenParser Char st Expr
lambdaDeclaration = do
    lambdaArgs <- lambdaFormalParameters
    _          <- matchTok '='
    lambdaExpr <- expression
    return $ LamE lambdaArgs lambdaExpr

lambdaFormalParameters :: GenParser Char st [Pattern]
lambdaFormalParameters = surroundedBy '(' (symbol `sepBy` matchTok ',') ')'
    where
        symbol :: ParsecT String st Identity Pattern
        symbol = do
            ident <- matchIdentifier
            return $ Name (Symbol ident)

-- Noteworthy: OpenSCAD allows one or more commas between the formal parameter declarations.
-- Many are treated as one. The last parameter declaration can be followed by commas, which are ignored.
oneOrMoreCommas :: GenParser Char st ()
oneOrMoreCommas = skipMany1 $ matchTok ','

returnDoNothing :: GenParser Char st StatementI
returnDoNothing = returnStatement DoNothing

returnStatement :: Statement StatementI -> GenParser Char st StatementI
returnStatement ast = do
    pos <- getPosition
    return $ StatementI (sourcePosition pos) ast

removeNoOps :: [StatementI] -> [StatementI]
removeNoOps [] = []
removeNoOps (StatementI _ (Sequence []):sts) = removeNoOps sts
removeNoOps (StatementI _ (Sequence [st]):sts) = removeNoOps [st] ++ removeNoOps sts
removeNoOps (StatementI _ DoNothing:sts) = removeNoOps sts
removeNoOps (st:sts) = st : removeNoOps sts

expression :: GenParser Char st Expr
expression = expr0
