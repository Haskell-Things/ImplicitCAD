-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Parser.AltLexer (lexer, matchOR, matchAND, matchLE, matchGE, matchEQ, matchNE, matchCAT, matchEach) where

-- Be explicit about what we import.
import Prelude (String, Char, return, (>>), Bool(True))
import Control.Monad.Identity (Identity)
import Text.Parsec.Token (GenTokenParser, makeTokenParser)
import Text.Parsec.String (GenParser)
import Text.Parsec ((<|>), char, letter, alphaNum)
import qualified Text.Parsec.Token as P (reserved, reservedOp)
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

matchEach :: GenParser Char st ()
matchEach = P.reserved lexer "each"

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

