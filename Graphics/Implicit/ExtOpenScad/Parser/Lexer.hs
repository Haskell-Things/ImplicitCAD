-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchFunction, matchInclude, matchUse, matchIf, matchElse, matchModule, matchLet, matchUndef, matchTok, matchColon, matchSemi, matchComma, matchIdentifier, surroundedBy, matchLT, matchLE, matchGT, matchGE, matchEQ, matchNE, matchCAT, matchOR, matchAND, matchEXP, matchEach, lexer) where

import Prelude (String, Char, Bool(True), (>>), pure, not, (&&), ($))

import Data.List (notElem)

import Data.Char (isSpace)

import Data.Functor.Identity (Identity)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec.Token as P (whiteSpace, reserved, identifier, reservedOp)

import Text.Parsec.Language (GenLanguageDef, emptyDef)

import Text.Parsec.Token (GenTokenParser, makeTokenParser, commentStart, commentEnd, commentLine, nestedComments, caseSensitive, colon, semi, comma, identStart, identLetter, reservedNames, reservedOpNames)

import Text.Parsec (char, between)

import Text.Parsec.Char (satisfy)

import Data.Text.Lazy (Text)

-- The definition of openscad used by parsec.
openScadStyle :: GenLanguageDef String u0 Identity
openScadStyle
    = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = True
    , identStart =  satisfy $ \c -> notElem c (",|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=1234567890" :: String) && not (isSpace c)
    , identLetter = satisfy $ \c -> notElem c (",|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=" :: String) && not (isSpace c)
    , reservedNames = ["module", "function", "if", "else", "let", "each", "true", "false", "undef", "include", "use"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||", "++", "^", "<", ">"]
    , caseSensitive = True
    }

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser openScadStyle

-- | Consume whitespace.
whiteSpace :: GenParser Char st ()
whiteSpace = P.whiteSpace lexer

-- | Match the module keyword.
matchModule :: GenParser Char st ()
matchModule = P.reserved lexer "module"

-- | Match the function keyword.
matchFunction :: GenParser Char st ()
matchFunction = P.reserved lexer "function"

-- | Match the if keyword.
matchIf :: GenParser Char st ()
matchIf = P.reserved lexer "if"

-- | Match the else keyword.
matchElse :: GenParser Char st ()
matchElse = P.reserved lexer "else"

-- | Match the let keyword.
matchLet :: GenParser Char st ()
matchLet = P.reserved lexer "let"

-- | Match the each keyword.
matchEach :: GenParser Char st ()
matchEach = P.reserved lexer "each"

-- | Match boolean true.
matchTrue :: GenParser Char st ()
matchTrue = P.reserved lexer "true"

-- | Match boolean false
matchFalse :: GenParser Char st ()
matchFalse = P.reserved lexer "false"

-- | Match the undef keyword.
matchUndef :: GenParser Char st ()
matchUndef = P.reserved lexer "undef"

-- | Match the include keyword.
matchInclude :: GenParser Char st ()
matchInclude = P.reserved lexer "include"

-- | Match the use keyword.
matchUse :: GenParser Char st ()
matchUse = P.reserved lexer "use"

-- | match a single character token followed by whitespace.
matchTok :: Char -> GenParser Char st Char
matchTok x = do
  y <- char x
  _ <- whiteSpace
  pure y
--matchTok tok = lexeme lexer $ symbol lexer [tok]

-- | match a colon.
matchColon :: GenParser Char st Text
matchColon = colon lexer >> pure ":"

-- | match a semicolon.
matchSemi :: GenParser Char st Text
matchSemi = semi lexer >> pure ";"

-- | match a comma.
matchComma :: GenParser Char st Text
matchComma = comma lexer >> pure ","

-- | Match operators.
matchLE :: GenParser Char st Text
matchLE = P.reservedOp lexer "<=" >> pure "<="
matchLT :: GenParser Char st Text
matchLT = P.reservedOp lexer "<" >> pure "<"
matchGE :: GenParser Char st Text
matchGE = P.reservedOp lexer ">=" >> pure ">="
matchGT :: GenParser Char st Text
matchGT = P.reservedOp lexer ">" >> pure ">"
matchEQ :: GenParser Char st Text
matchEQ = P.reservedOp lexer "==" >> pure "=="
matchNE :: GenParser Char st Text
matchNE = P.reservedOp lexer "!=" >> pure "!="
matchAND :: GenParser Char st Text
matchAND = P.reservedOp lexer "&&" >> pure "&&"
matchOR :: GenParser Char st Text
matchOR = P.reservedOp lexer "||" >> pure "||"
matchCAT :: GenParser Char st Text
matchCAT = P.reservedOp lexer "++" >> pure "++"
matchEXP :: GenParser Char st Char
matchEXP = P.reservedOp lexer "^" >> pure '^'

-- | match something between two ends.
surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

-- | match an identifier. variable name, function name, module name, etc.
matchIdentifier :: GenParser Char st String
matchIdentifier = P.identifier lexer
