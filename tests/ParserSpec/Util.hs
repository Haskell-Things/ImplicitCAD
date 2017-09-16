-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Utilities
module ParserSpec.Util
       ( num
       , bool
       , app
       , app'
       , parseWithEof
       , parseWithLeftOver
       , parseExpr
       ) where

-- be explicit about where we get things from.
import Prelude (Bool, String, Either, (<), ($), (.), otherwise)

-- The datatype of positions in our world.
import Graphics.Implicit.Definitions (ℝ)

-- The datatype of expressions, symbols, and values in the OpenScad language.
import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LitE, (:$), Var, ListE), Symbol, OVal(ONum, OBool))

-- the entry point of the expression parser.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)
    
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, manyTill, anyChar, eof)

import Control.Applicative ((<$>), (<*>), (<*))

num :: ℝ -> Expr
num x
  -- FIXME: the parser should handle negative number literals
  -- directly, we abstract that deficiency away here
  | x < 0 = app' "negate" [LitE $ ONum (-x)]
  | otherwise = LitE $ ONum x

bool :: Bool -> Expr
bool = LitE . OBool

-- Operators and functions need two different kinds of applications
app :: String -> [Expr] -> Expr
app name args = Var name :$ [ListE args]

app' :: Symbol -> [Expr] -> Expr
app' name args = Var name :$ args

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where
    leftOver :: Parser String
    leftOver = manyTill anyChar eof

parseWithEof :: Parser a -> String -> String -> Either ParseError a
parseWithEof p = parse (p <* eof)

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWithEof expr0 "expr"
