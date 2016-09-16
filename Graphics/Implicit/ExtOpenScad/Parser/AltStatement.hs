module Graphics.Implicit.ExtOpenScad.Parser.AltStatement (
    parseProgram,
    altParseProgram
) where

import Data.Maybe
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer
import Graphics.Implicit.ExtOpenScad.Parser.AltExpr
import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)

-- In general, the grammar is predictive and requires no more than one character of lookahead,
-- except when differentiating between keywords and identifiers.
-- Or at least, that is the goal. exceptions are documented.

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

statements :: GenParser Char st [StatementI]
statements = removeNoOps <$> many1 statement

statement :: GenParser Char st StatementI
statement =
        do
        _ <- oneOrMoreSemis
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
        moduleName <- identifier
        argDecls   <- surroundedBy '(' moduleParametersDeclaration ')'
        stmt       <- statements
        returnStatement $ NewModule moduleName argDecls stmt
    <|> do -- user function declaration, e.g. "function foo(a, b = [1,2]) a;
        _          <- matchFunction
        funcName   <- identifier
        argDecls   <- surroundedBy '(' argumentsDeclaration ')'
        _          <- matchTok '='
        funcExpr   <- expression
        _          <- oneOrMoreSemis
        returnStatement $ NewFunction funcName argDecls funcExpr
    <|>
        ifStatement

ifStatement = do
    _          <- matchIf
    condition  <- surroundedBy '(' expression ')'
    trueScope  <- statements
    falseMaybe <- optionMaybe $ matchElse >> statements
    returnStatement $ If condition trueScope $ fromMaybe [] falseMaybe

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
flaggedModuleInstantiation = do
        moduleFlag '!' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '#' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '%' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '*' maybeFlaggedModuleInstantiation
        returnDoNothing
    where moduleFlag tok inst = matchTok tok >> inst

maybeFlaggedModuleInstantiation :: GenParser Char st StatementI
maybeFlaggedModuleInstantiation = do
        moduleFlag '!' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '#' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '%' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleFlag '*' maybeFlaggedModuleInstantiation
        returnDoNothing
    <|> do
        moduleName <- identifier
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
        ifStatement
        returnDoNothing
    where moduleFlag tok inst = matchTok tok >> inst

moduleInstantiationTail :: Symbol -> GenParser Char st StatementI
moduleInstantiationTail moduleName = do
    arguments  <- surroundedBy '(' moduleCallArguments ')'
    child      <- childStatement
    returnStatement $ ModuleCall moduleName arguments $ removeNoOps [child]

moduleParametersDeclaration :: GenParser Char st [(Symbol, Maybe Expr)]
moduleParametersDeclaration = sepEndBy moduleParameterDeclaration oneOrMoreCommas

moduleParameterDeclaration :: GenParser Char st (Symbol, Maybe Expr)
moduleParameterDeclaration = do
    parameterName <- identifier
    defaultExpr   <- optionMaybe defaultExpression
    return (parameterName, defaultExpr)
    where
        defaultExpression =
              lambdaDeclaration
          <|> (matchTok '=' >> expression)

argumentsDeclaration :: GenParser Char st [(Symbol, Maybe Expr)]
argumentsDeclaration = sepEndBy argumentDeclaration oneOrMoreCommas

argumentDeclaration :: GenParser Char st (Symbol, Maybe Expr)
argumentDeclaration = do
    parameterName <- identifier
    defaultExpr   <- optionMaybe defaultValueExpression
    return (parameterName, defaultExpr)
    where defaultValueExpression = matchTok '=' >> expression

childStatements :: GenParser Char st [StatementI]
childStatements = removeNoOps <$> many innerChildStatement

childStatement :: GenParser Char st StatementI
childStatement =
        do
        _ <- oneOrMoreSemis
        returnDoNothing
    <|> do
        statements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence statements
    <|>
        maybeFlaggedModuleInstantiation

innerChildStatement :: GenParser Char st StatementI
innerChildStatement =
        do
        _ <- oneOrMoreSemis
        returnDoNothing
    <|> do
        statements <- surroundedBy '{' childStatements '}'
        returnStatement $ Sequence statements
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
        paramName <- identifier
        do
            _        <- matchTok '='
            argValue <- expression
            return (Just paramName, argValue)
         <|> do
            lambda <- lambdaDeclaration
            return (Just paramName, lambda))
    <|> do
        argValue  <- expression
        return (Nothing, argValue)

lambdaDeclaration :: GenParser Char st Expr
lambdaDeclaration = do
    lambdaArgs <- lambdaFormalParameters
    _          <- matchTok '='
    lambdaExpr <- expression
    return $ LamE lambdaArgs lambdaExpr

lambdaFormalParameters = surroundedBy '(' (symbol `sepBy` matchTok ',') ')'
    where
        symbol = do
            ident <- identifier
            return $ Name ident

-- Noteworthy: OpenSCAD allows one or more commas between the formal parameter declarations.
-- Many are treated as one. The last parameter declaration can be followed by commas, which are ignored.
oneOrMoreCommas :: GenParser Char st ()
oneOrMoreCommas = skipMany1 $ matchTok ','

oneOrMoreSemis :: GenParser Char st ()
oneOrMoreSemis =  skipMany1 $ matchTok ';'

surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

returnDoNothing :: GenParser Char st StatementI
returnDoNothing = returnStatement DoNothing

returnStatement :: Statement StatementI -> GenParser Char st StatementI
returnStatement ast = do
    pos <- getPosition
    return $ StatementI (sourcePosition pos) ast

removeNoOps :: [StatementI] -> [StatementI]
removeNoOps [] = []
removeNoOps a@(StatementI _ (Sequence []):sts) = removeNoOps sts
removeNoOps a@(StatementI _ (Sequence [st]):sts) = removeNoOps [st] ++ removeNoOps sts
removeNoOps a@(StatementI _ DoNothing:sts) = removeNoOps sts
removeNoOps (st:sts) = st : removeNoOps sts

expression :: GenParser Char st Expr
expression = altExpr
