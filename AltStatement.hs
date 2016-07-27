module Graphics.Implicit.ExtOpenScad.Parser.AltStatement (
    parseProgram,
    altParseProgram
) where

import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer

altParseProgram :: SourceName -> String -> Either ParseError [StatementI]
altParseProgram = parseProgram

parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program where
    program = do
        sts <- many1 computation
        eof
        return sts

computation :: GenParser Char st StatementI
computation =
    do
        _ <- whiteSpace
        statement

statement :: GenParser Char st StatementI
statement =
    do
        line <- lineNumber
        _ <- matchTok ';'
        return $ StatementI line DoNothing

lineNumber :: GenParser Char m Line
lineNumber = fmap sourceLine getPosition
