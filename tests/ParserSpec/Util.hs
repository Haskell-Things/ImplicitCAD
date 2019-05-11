-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Utilities
module ParserSpec.Util
       ( (-->)
       , (-->+)
       , num
       , bool
       , stringLiteral
       , fapp
       , plus
       , minus
       , mult
       , modulo
       , power
       , divide
       , not
       , and
       , or
       , gt
       , lt
       , negate
       , ternary
       , append
       , index
       , parseWithLeftOver
       ) where

-- be explicit about where we get things from.
import Prelude (Bool, String, Either, (<), ($), (.), (<*), otherwise)

-- The datatype of positions in our world.
import Graphics.Implicit.Definitions (ℝ)

-- The datatype of expressions, symbols, and values in the OpenScad language.
import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LitE, (:$), Var, ListE), OVal(ONum, OBool, OString))

import Text.ParserCombinators.Parsec (Parser, ParseError, parse, manyTill, anyChar, eof)

import Control.Applicative ((<$>), (<*>))

import Test.Hspec (Expectation, shouldBe)

import Data.Either (Either(Right))

-- the expression parser entry point.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- An operator for expressions for "the left side should parse to the right side."
infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  parse (expr0 <* eof) "<expr>" source `shouldBe` Right expr

-- An operator for expressions for "the left side should parse to the right side, and some should be left over".
infixr 1 -->+
(-->+) :: String -> (Expr, String) -> Expectation
(-->+) source (result, leftover) =
  parseWithLeftOver expr0 source `shouldBe` Right (result, leftover)

num :: ℝ -> Expr
num x
  -- FIXME: the parser should handle negative number literals
  -- directly, we abstract that deficiency away here
  | x < 0 = oapp "negate" [LitE $ ONum (-x)]
  | otherwise = LitE $ ONum x

bool :: Bool -> Expr
bool = LitE . OBool

stringLiteral :: String -> Expr
stringLiteral = LitE . OString

plus,minus,mult,modulo,power,divide,negate,and,or,not,gt,lt,ternary,append,index :: [Expr] -> Expr
minus = oapp "-"
modulo = oapp "%"
power = oapp "^"
divide = oapp "/"
and = oapp "&&"
or = oapp "||"
not = oapp "!"
gt = oapp ">"
lt = oapp "<"
ternary = oapp "?"
negate = oapp "negate"
index = oapp "index"
plus = fapp "+"
mult = fapp "*"
append = fapp "++"

-- we need two different kinds of application functions
oapp,fapp :: String -> [Expr] -> Expr
oapp name args = Var name :$ args
fapp name args = Var name :$ [ListE args]

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where
    leftOver :: Parser String
    leftOver = manyTill anyChar eof
