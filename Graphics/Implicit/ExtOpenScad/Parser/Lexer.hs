-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchFunction, matchInclude, matchUse, matchEcho, matchIf, matchElse, matchFor, matchModule, matchLet, matchUndef, matchTok, matchColon, matchComma, matchIdentifier, surroundedBy) where

import Prelude (String, Char, Bool(True), return)

import Control.Monad.Identity (Identity)

import Text.Parsec.Token (GenTokenParser, makeTokenParser)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec.Token as P (whiteSpace, reserved, identifier)

import Text.Parsec.Language (GenLanguageDef, emptyDef)

import Text.Parsec.Token (commentStart, commentEnd, commentLine, nestedComments, caseSensitive, colon, comma, identStart, identLetter, reservedNames)

import Text.Parsec (char, between)

import Text.Parsec.Char (noneOf)

-- The definition of openscad used by parsec.
openScadStyle :: GenLanguageDef String u0 Identity
openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart =  noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=1234567890"
    , identLetter = noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?="
    , reservedNames = ["module", "function", "if", "else", "let", "for", "each", "true", "false", "undef", "include", "use"]
    , caseSensitive = True
    }

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser openScadStyle

-- | Consume whitespace.
whiteSpace :: GenParser Char st ()
whiteSpace = P.whiteSpace lexer

-- | Match boolean true.
matchTrue :: GenParser Char st ()
matchTrue = P.reserved lexer "true"

-- | Match boolean false
matchFalse :: GenParser Char st ()
matchFalse = P.reserved lexer "false"

-- | Match the function keyword.
matchFunction :: GenParser Char st ()
matchFunction = P.reserved lexer "function"

-- | Match the include keyword.
matchInclude :: GenParser Char st ()
matchInclude = P.reserved lexer "include"

-- | Match the use keyword.
matchUse :: GenParser Char st ()
matchUse = P.reserved lexer "use"

-- | Match the echo keyword.
matchEcho :: GenParser Char st ()
matchEcho = P.reserved lexer "echo"

-- | Match the if keyword.
matchIf :: GenParser Char st ()
matchIf = P.reserved lexer "if"

-- | Match the else keyword.
matchElse :: GenParser Char st ()
matchElse = P.reserved lexer "else"

-- | Match the for keyword.
matchFor :: GenParser Char st ()
matchFor = P.reserved lexer "for"

-- | Match the module keyword.
matchModule :: GenParser Char st ()
matchModule = P.reserved lexer "module"

-- | Match the let keyword.
matchLet :: GenParser Char st ()
matchLet = P.reserved lexer "let"

-- | Match the undef keyword.
matchUndef :: GenParser Char st ()
matchUndef = P.reserved lexer "undef"

-- | match a single character token followed by whitespace.
matchTok :: Char -> GenParser Char st String
matchTok x = do
  y <- char x
  _ <- P.whiteSpace lexer
  return [y]
--matchTok tok = lexeme lexer $ symbol lexer [tok]

-- | match a colon.
matchColon :: GenParser Char st String
matchColon = colon lexer

-- | match a comma.
matchComma :: GenParser Char st String
matchComma = comma lexer

-- | match something between two ends.
surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

matchIdentifier :: GenParser Char st String
matchIdentifier = P.identifier lexer
