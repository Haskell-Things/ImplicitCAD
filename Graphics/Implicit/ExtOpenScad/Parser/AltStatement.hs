module Graphics.Implicit.ExtOpenScad.Parser.AltStatement (
    parseProgram,
    altParseProgram
) where

import Data.Maybe
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer
import Graphics.Implicit.ExtOpenScad.Parser.AltExpr

-- The structure of this file tries to follow the ordering of rules in the OpenSCAD parser.y,
-- in order to support easier comparison and maintenance.
-- The grammar is predictive and requires no more than one character of lookahead,
-- except when differentiating between keywords and identifiers.
-- Or at least, that is the goal.

altParseProgram :: SourceName -> String -> Either ParseError [StatementI]
altParseProgram = parseProgram

parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program where
    program = do
         -- all of the token parsers are lexemes which consume all trailing spaces nicely.
         -- This leaves us to deal only with the first spaces in the file.
        _    <- whiteSpace
        prog <- input
        _    <- eof
        return prog

input :: GenParser Char st [StatementI]
input = many1 statement

statement :: GenParser Char st StatementI
statement =
        do
        _ <- oneOrMoreSemis
        returnStatement DoNothing
    <|> do
        stmts <- surroundedBy '{' (many statement) '}'
        returnStatement $ Sequence stmts
    <|> -- things starting with an identifier
        moduleInstantiationOrAssignment
    <|> -- module instantiations with '!', '#', '%', '*' prefixes
        flaggedModuleInstantiation
    <|> do -- user module declaration, e.g. "module foo(a, b = [1,2]) { }
        _          <- matchModule
        moduleName <- identifier
        argDecls   <- surroundedBy '(' argumentsDeclaration ')'
        stmt       <- statement
        returnStatement $ NewModule moduleName argDecls [stmt]
    <|> do -- user function declaration, e.g. "function foo(a, b = [1,2]) a;
        _          <- matchFunction
        funcName   <- identifier
        argDecls   <- surroundedBy '(' argumentsDeclaration ')'
        _          <- matchTok '='
        funcExpr   <- expression
        _          <- oneOrMoreSemis
        returnStatement $ NewFunction funcName argDecls funcExpr
    <|> do
        _          <- matchIf
        condition  <- surroundedBy '(' expression ')'
        trueScope  <- statement
        falseMaybe <- optionMaybe $ matchElse >> statement
        n <- nothing
        returnStatement $ If condition [trueScope] [fromMaybe n falseMaybe]

nothing :: GenParser Char st StatementI
nothing = returnStatement DoNothing

flattenStatement :: GenParser Char st [StatementI]
flattenStatement = do
    stmts <- statement
    case stmts of StatementI _ DoNothing -> return []
                  StatementI _ (Sequence [sts]) -> return [sts]
                  _ -> return [stmts]

-- Assignment and module instantiation both start with the same token, an identifier.
-- So, in order to keep the parser predictive (for performance reasons),
-- the grammar is factored to first recognize the identifier, and choose between
-- assignment and module instantiation based on the following token.
-- The module modification flags, '!', '#', '%', '*', will need to be handled in a separate
-- parser.
moduleInstantiationOrAssignment :: GenParser Char st StatementI
moduleInstantiationOrAssignment = do
        ident <- identifier
        moduleInstantiationTail ident <|> assignment ident
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
    _     <- oneOrMoreSemis
    returnStatement $ Name ident := expr

flaggedModuleInstantiation :: GenParser Char st StatementI
flaggedModuleInstantiation =
        moduleFlag '!' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '#' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '%' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '*' maybeFlaggedModuleInstantiation
    where moduleFlag tok inst = matchTok tok >> inst

maybeFlaggedModuleInstantiation :: GenParser Char st StatementI
maybeFlaggedModuleInstantiation =
        moduleFlag '!' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '#' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '%' maybeFlaggedModuleInstantiation
    <|>
        moduleFlag '*' maybeFlaggedModuleInstantiation
    <|> do
        moduleName <- identifier
        moduleInstantiationTail moduleName
    where moduleFlag tok inst = matchTok tok >> inst

moduleInstantiationTail :: Symbol -> GenParser Char st StatementI
moduleInstantiationTail moduleName = do
    arguments  <- surroundedBy '(' argumentsCall ')'
    child      <- childStatement
    returnStatement $ ModuleCall moduleName arguments [child]

argumentsDeclaration :: GenParser Char st [(Symbol, Maybe Expr)]
argumentsDeclaration = sepEndBy argumentDeclaration oneOrMoreCommas

argumentDeclaration :: GenParser Char st (Symbol, Maybe Expr)
argumentDeclaration = do
    parameterName <- identifier
    defaultExpr   <- optionMaybe defaultValueExpression
    return (parameterName, defaultExpr)
    where defaultValueExpression = matchTok '=' >> expression

childStatements :: GenParser Char st [StatementI]
childStatements = many innerChildStatement

childStatement :: GenParser Char st StatementI
childStatement =
        do
        _ <- oneOrMoreSemis
        returnStatement DoNothing
    <|> do
        statements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence statements
    <|>
        maybeFlaggedModuleInstantiation

innerChildStatement :: GenParser Char st StatementI
innerChildStatement =
        do
        _ <- oneOrMoreSemis
        returnStatement DoNothing
    <|> do
        statements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence statements
    <|>
        moduleInstantiationOrAssignment
    <|> -- module instantiations with '!', '#', '%', '*' prefixes
        flaggedModuleInstantiation

argumentsCall :: GenParser Char st [(Maybe Symbol, Expr)]
argumentsCall = sepEndBy argumentCall oneOrMoreCommas

argumentCall :: GenParser Char st (Maybe Symbol, Expr)
argumentCall = do
    argValue  <- expression
    do
        _ <- matchTok '='
        expr <- expression
        case argValue of --Var paramName :$ lambdaParams -> return (Just paramName, LamE lambdaParams expr)
                         Var paramName -> return (Just paramName, expr)
                         _ -> return (Nothing, argValue)
      <|>
        return (Nothing, argValue)

--Var "f" :$ [LitE 1.0,LitE 2.0]))]
--LamE [Name boundName] nestedExpr :$ [boundExpr]

-- Noteworthy: OpenSCAD allows one or more commas between the formal parameter declarations.
-- Many are treated as one. The last parameter declaration can be followed by commas, which are ignored.
oneOrMoreCommas :: GenParser Char st ()
oneOrMoreCommas = skipMany1 $ matchTok ','

oneOrMoreSemis :: GenParser Char st ()
oneOrMoreSemis =  skipMany1 $ matchTok ';'

surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

returnStatement :: Statement StatementI -> GenParser Char st StatementI
returnStatement ast = do
    line <- fmap sourceLine getPosition
    return $ StatementI line ast

expression :: GenParser Char st Expr
expression = altExpr
