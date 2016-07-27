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
        line <- lineNumber
        _    <- many1 (matchTok ';')
        return $ StatementI line DoNothing
    <|> do
        line  <- lineNumber
        _     <- matchTok '{'
        stmts <- many statement
        _     <- matchTok '}'
        return $ StatementI line $ Sequence stmts
    <|>
        assignment
    <|> do -- user module declaration, e.g. "module foo(a, b = [1,2]) { }
        line       <- lineNumber
        _          <- matchModule
        moduleName <- identifier
        _          <- matchTok '('
        argDecls   <- argumentsDeclaration
        _          <- matchTok ')'
        stmt       <- statement
        return $ StatementI line $ NewModule moduleName argDecls [stmt]

-- An assignment statement, not a default value assignment in formal parameters or an argument assignment in calling a module.
assignment :: GenParser Char st StatementI
assignment =
    do
        line  <- lineNumber
        ident <- identifier
        _     <- matchTok '='
        expr  <- altExpr
        _     <- matchTok ';'
        return $ StatementI line $ Name ident := expr

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

lineNumber :: GenParser Char m Line
lineNumber = fmap sourceLine getPosition
