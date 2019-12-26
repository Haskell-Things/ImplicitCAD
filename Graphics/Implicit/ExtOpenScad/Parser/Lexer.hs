-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

module Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchFunction, matchInclude, matchUse, matchIf, matchElse, matchModule, matchLet, matchUndef, matchTok, matchColon, matchSemi, matchComma, matchIdentifier, surroundedBy, matchLT, matchLE, matchGT, matchGE, matchEQ, matchNE, matchCAT, matchOR, matchAND, matchEach, lexer) where

import Prelude (String, Char, Bool(True), (>>), pure)

import "monads-tf" Control.Monad.Identity (Identity)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec.Token as P (whiteSpace, reserved, identifier, reservedOp)

import Text.Parsec.Language (GenLanguageDef, emptyDef)

import Text.Parsec.Token (GenTokenParser, makeTokenParser, commentStart, commentEnd, commentLine, nestedComments, caseSensitive, colon, semi, comma, identStart, identLetter, reservedNames, reservedOpNames)

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
    , reservedNames = ["module", "function", "if", "else", "let", "each", "true", "false", "undef", "include", "use"]
    , reservedOpNames= ["<=", ">=", "==", "!=", "&&", "||"]
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

-- | Match the if keyword.
matchIf :: GenParser Char st ()
matchIf = P.reserved lexer "if"

-- | Match the else keyword.
matchElse :: GenParser Char st ()
matchElse = P.reserved lexer "else"

-- | Match the module keyword.
matchModule :: GenParser Char st ()
matchModule = P.reserved lexer "module"

-- | Match the let keyword.
matchLet :: GenParser Char st ()
matchLet = P.reserved lexer "let"

-- | Match the undef keyword.
matchUndef :: GenParser Char st ()
matchUndef = P.reserved lexer "undef"

-- | Match the each keyword.
matchEach :: GenParser Char st ()
matchEach = P.reserved lexer "each"

-- | match a single character token followed by whitespace.
matchTok :: Char -> GenParser Char st String
matchTok x = do
  y <- char x
  _ <- P.whiteSpace lexer
  pure [y]
--matchTok tok = lexeme lexer $ symbol lexer [tok]

-- | match a colon.
matchColon :: GenParser Char st String
matchColon = colon lexer

-- | match a semicolon.
matchSemi :: GenParser Char st String
matchSemi = semi lexer

-- | match a comma.
matchComma :: GenParser Char st String
matchComma = comma lexer

-- | Match operators.
matchLE :: GenParser Char st String
matchLE = P.reservedOp lexer "<=" >> pure "<="
matchLT :: GenParser Char st String
matchLT = matchTok '<'
matchGE :: GenParser Char st String
matchGE = P.reservedOp lexer ">=" >> pure ">="
matchGT :: GenParser Char st String
matchGT = matchTok '>'
matchEQ :: GenParser Char st String
matchEQ = P.reservedOp lexer "==" >> pure "=="
matchNE :: GenParser Char st String
matchNE = P.reservedOp lexer "!=" >> pure "!="
matchAND :: GenParser Char st String
matchAND = P.reservedOp lexer "&&" >> pure "&&"
matchOR :: GenParser Char st String
matchOR = P.reservedOp lexer "||" >> pure "||"
matchCAT :: GenParser Char st String
matchCAT = P.reservedOp lexer "++" >> pure "++"


-- | match something between two ends.
surroundedBy :: Char -> GenParser Char st a -> Char -> GenParser Char st a
surroundedBy leftTok middle rightTok = between (matchTok leftTok) (matchTok rightTok) middle

-- | match an identifier. variable name, function name, module name, etc.
matchIdentifier :: GenParser Char st String
matchIdentifier = P.identifier lexer
