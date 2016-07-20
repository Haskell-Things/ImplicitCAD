module Graphics.Implicit.ExtOpenScad.Parser.Lexer where

import Debug.Trace;
import Text.Parsec.Token
import Text.ParserCombinators.Parsec  hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- The token parsers are in roughly the same order as the OpenSCAD lexer.l Flex source, to make it easier to compare them.

openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart = letter <|> char '$' <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , reservedNames = ["module", "function", "if", "else", "let", "for", "each", "true", "false", "undef"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||"]
    , caseSensitive = True
    }

lexer = makeTokenParser
        (openScadStyle)

-- Deal with unicode later.

matchModule = P.reserved lexer "module"
matchFunction = P.reserved lexer "function"
matchIf = P.reserved lexer "if"
matchElse = P.reserved lexer "else"
matchLet = P.reserved lexer "let"
matchFor = P.reserved lexer "for"
matchEach = P.reserved lexer "each"
matchTrue = P.reserved lexer "true"
matchFalse = P.reserved lexer "false"
matchUndef = P.reserved lexer "undef"

number = do
        do
            _ <- char '.' <?> "" -- OpenSCAD supports floating point numbers that start with a decimal.
            fractional <- (many1 digit)
            exp <- (
                do
                    _ <- oneOf "eE"
                    sign <- ( (oneOf "-+" <?> "exponent") <|> return '+')
                    exp <- (many1 digit) <?> "exponent"
                    return $ 'e':sign:exp
                <|> 
                    (return $ "")
                )
            return $ Right $ ((read $ "0." ++ fractional ++ exp) :: Double)
    <|>
        P.naturalOrFloat lexer

identifier = P.identifier lexer
literalString = P.stringLiteral lexer -- not sure if the Parsec idea of string literal matches OpenSCAD. Consider unicode, \x hex codes

-- single line comments, multiline comments and whitespace are consumed by the other token types, since the parser doesn't really need them to build the AST

matchLE = P.reservedOp lexer "<=" >> return "<="
matchGE = P.reservedOp lexer ">=" >> return ">="
matchEQ = P.reservedOp lexer "==" >> return "=="
matchNE = P.reservedOp lexer "!=" >> return "!="
matchAND = P.reservedOp lexer "&&" >> return "&&"
matchOR = P.reservedOp lexer "||" >> return "||"
matchCAT = P.reservedOp lexer "++" >> return "++"

-- single character tokens can be handled fine in the main parser, just strip the trailing whitespace.
matchChar char = P.lexeme lexer $ P.symbol lexer [char]
