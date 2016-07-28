module Graphics.Implicit.ExtOpenScad.Parser.AltStatement (
    parseProgram,
    altParseProgram
) where

import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer
import Graphics.Implicit.ExtOpenScad.Parser.AltExpr

-- The structure of this file tries to follow the ordering of rules in the OpenSCAD parser.y,
-- in order to support easier comparison and maintenance.
-- The grammar is predictive and requires no more than one character of lookahead.
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
        _ <- many1 (matchTok ';')
        returnStatement DoNothing
    <|> do
        stmts <- surroundedBy '{' (many statement) '}'
        returnStatement $ Sequence stmts
    <|>
        assignment
    <|> do -- user module declaration, e.g. "module foo(a, b = [1,2]) { }
        _          <- matchModule
        moduleName <- identifier
        argDecls   <- surroundedBy '(' argumentsDeclaration ')'
        stmt       <- statement
        returnStatement $ NewModule moduleName argDecls [stmt]

-- An assignment statement, not a default value assignment in formal parameters or an argument assignment in calling a module.
assignment :: GenParser Char st StatementI
assignment =
    do
        ident <- identifier
        _     <- matchTok '='
        expr  <- altExpr
        _     <- matchTok ';'
        returnStatement $ Name ident := expr

-- Noteworthy: OpenSCAD allows one or more commas between the formal parameter declarations.
-- Many are treated as one. The last parameter declaration can be followed by commas, which are ignored.
argumentsDeclaration :: GenParser Char st [(String, Maybe Expr)]
argumentsDeclaration = sepEndBy argumentDeclaration (skipMany1 $ matchTok ',')

argumentDeclaration :: GenParser Char st (String, Maybe Expr)
argumentDeclaration =
    do
        parameterName          <- identifier
        defaultValueExpression <- optionMaybe (matchTok '=' >> altExpr)
        return (parameterName, defaultValueExpression)

surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

returnStatement :: Statement StatementI -> GenParser Char st StatementI
returnStatement ast = do
    line <- fmap sourceLine getPosition
    return $ StatementI line ast
