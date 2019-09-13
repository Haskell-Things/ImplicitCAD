-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Utilities
module ParserSpec.Util
       ( (-->)
       , num
       , bool
       , stringLiteral
       , undefined
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
       , lambda
       , parseWithLeftOver
       ) where

-- be explicit about where we get things from.
import Prelude (Bool, String, Either, (<), ($), (.), (<*), otherwise)

-- The datatype of positions in our world.
import Graphics.Implicit.Definitions (ℝ)

-- Expressions, symbols, and values in the OpenScad language.
import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LitE, (:$), Var, ListE, LamE), Symbol(Symbol), OVal(ONum, OBool, OString, OUndefined), Pattern)

import Text.Parsec (ParseError, parse, manyTill, anyChar, eof)

import Text.Parsec.String (Parser)

import Control.Applicative ((<$>), (<*>))

import Test.Hspec (Expectation, shouldBe)

import Data.Either (Either(Right))

-- The expression parser entry point.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

-- An operator for expressions for "the left side should parse to the right side."
infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  parse (expr0 <* eof) "<expr>" source `shouldBe` Right expr

-- | Types

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

undefined :: Expr
undefined = LitE OUndefined

-- | Operators

plus,minus,mult,modulo,power,divide,negate,and,or,not,gt,lt,ternary,append,index :: [Expr] -> Expr
minus = oapp "-"
modulo = oapp "%"
power = oapp "^"
mult = oapp "*"
divide = oapp "/"
and = oapp "&&"
or = oapp "||"
not = oapp "!"
gt = oapp ">"
lt = oapp "<"
ternary = oapp "?"
negate = oapp "negate"
index = oapp "index"
append = oapp "++"
plus = oapp "+"

-- | We need two different kinds of application functions, one for operators, and one for functions.
oapp,fapp :: String -> [Expr] -> Expr
oapp name args = Var (Symbol name) :$ args
fapp name args = Var (Symbol name) :$ [ListE args]

lambda :: [Pattern] -> Expr -> [Expr] -> Expr
lambda params expr args = LamE params expr :$ args

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where
    leftOver :: Parser String
    leftOver = manyTill anyChar eof
