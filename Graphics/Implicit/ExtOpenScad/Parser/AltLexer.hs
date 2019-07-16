-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Parser.AltLexer (lexer, matchTok, matchUndef, number, literalString, identifier, matchOR, matchAND, matchLE, matchGE, matchEQ, matchNE, matchCAT, matchLet, matchModule, matchFunction, matchIf, matchElse, matchFor, matchEach) where

-- Be explicit about what we import.
import Prelude (String, Char, Either(Right), Integer, Double, return, (>>), Bool(True), ($), (++), read)
import Control.Monad.Identity (Identity)
import Text.Parsec.Token (GenTokenParser, makeTokenParser)
import Text.Parsec.String (GenParser)
import Text.Parsec ((<|>), (<?>), char, letter, alphaNum, digit, many1, oneOf)
import qualified Text.Parsec.Token as P (reserved, naturalOrFloat, identifier, stringLiteral, reservedOp, lexeme, symbol)
import Text.Parsec.Language (GenLanguageDef, emptyDef)
import Text.Parsec.Token (commentStart, commentEnd, commentLine, nestedComments, identStart, identLetter, reservedNames, reservedOpNames, caseSensitive)

-- The token parsers are in roughly the same order as the OpenSCAD lexer.l Flex source, to make it easier to compare them.

openScadStyle :: GenLanguageDef String u0 Identity
openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart = letter <|> char '$' <|> char '_'
    , identLetter = alphaNum <|> char '_'
    , reservedNames = ["module", "function", "if", "else", "let", "for", "each", "true", "false", "undef", "include", "use"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||"]
    , caseSensitive = True
    }

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser openScadStyle

-- Deal with unicode later.

matchModule :: GenParser Char st ()
matchModule = P.reserved lexer "module"
matchFunction :: GenParser Char st ()
matchFunction = P.reserved lexer "function"
matchIf :: GenParser Char st ()
matchIf = P.reserved lexer "if"
matchElse :: GenParser Char st ()
matchElse = P.reserved lexer "else"
matchLet :: GenParser Char st ()
matchLet = P.reserved lexer "let"
matchFor :: GenParser Char st ()
matchFor = P.reserved lexer "for"
matchEach :: GenParser Char st ()
matchEach = P.reserved lexer "each"
matchUndef :: GenParser Char st ()
matchUndef = P.reserved lexer "undef"

number :: GenParser Char st (Either Integer Double)
number =
    do
        _ <- char '.' <?> "" -- OpenSCAD supports floating point numbers that start with a decimal.
        fractional <- many1 digit
        expont <-
            do
                _ <- oneOf "eE"
                sign <- (oneOf "-+" <?> "exponent") <|> return '+'
                expo <- many1 digit <?> "exponent"
                return $ 'e':sign:expo
            <|>
                return ""
        return $ Right ((read $ "0." ++ fractional ++ expont) :: Double)
    <|>
        P.naturalOrFloat lexer

identifier :: GenParser Char st String
identifier = P.identifier lexer

literalString :: GenParser Char st String
literalString = P.stringLiteral lexer -- not sure if the Parsec idea of string literal matches OpenSCAD. Consider unicode, \x hex codes

-- single line comments, multiline comments and whitespace are consumed by the other token types, since the parser doesn't really need them to build the AST

matchLE :: GenParser Char st String
matchLE = P.reservedOp lexer "<=" >> return "<="
matchGE :: GenParser Char st String
matchGE = P.reservedOp lexer ">=" >> return ">="
matchEQ :: GenParser Char st String
matchEQ = P.reservedOp lexer "==" >> return "=="
matchNE :: GenParser Char st String
matchNE = P.reservedOp lexer "!=" >> return "!="
matchAND :: GenParser Char st String
matchAND = P.reservedOp lexer "&&" >> return "&&"
matchOR :: GenParser Char st String
matchOR = P.reservedOp lexer "||" >> return "||"
matchCAT :: GenParser Char st String
matchCAT = P.reservedOp lexer "++" >> return "++"

-- single character tokens can be handled fine in the main parser, just strip the trailing whitespace.
matchTok :: Char -> GenParser Char st String
matchTok tok = P.lexeme lexer $ P.symbol lexer [tok]
