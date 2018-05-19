-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module ParserSpec.Expr (exprSpec) where

-- Be explicit about what we import.
import Prelude (String, Bool(True, False), ($), (<*), )

-- Hspec, for writing specs.
import Test.Hspec (describe, Expectation, Spec, it, shouldBe, pendingWith, specify)

-- parsed expression components.
import Graphics.Implicit.ExtOpenScad.Definitions (Expr(Var, ListE, (:$)) )

-- the expression parser entry point.
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)

import ParserSpec.Util (fapp, num, bool, plus, minus, mult, modulo, power, divide, negate, and, or, gt, lt, ternary, append, index, parseWithLeftOver)

import Data.Either (Either(Right))

import Text.ParserCombinators.Parsec (parse, eof)

-- An operator for expressions for "the left side should parse to the right side."
infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  parse (expr0 <* eof) "<expr>" source `shouldBe` Right expr

-- An operator for expressions for "the left side should parse to the right side, and some should be left over.
infixr 1 -->+
(-->+) :: String -> (Expr, String) -> Expectation
(-->+) source (result, leftover) =
  parseWithLeftOver expr0 source `shouldBe` Right (result, leftover)

ternaryIssue :: Expectation -> Expectation
ternaryIssue _ = pendingWith "parser doesn't handle ternary operator correctly"

negationIssue :: Expectation -> Expectation
negationIssue _ = pendingWith "parser doesn't handle negation operator correctly"

logicalSpec :: Spec
logicalSpec = do
  describe "not" $ do
    specify "single" $ "!foo" --> negate [Var "foo"]
    specify "multiple" $
      negationIssue $ "!!!foo" --> negate [negate [negate [Var "foo"]]]
  it "handles and/or" $ do
    "foo && bar" --> and [Var "foo", Var "bar"]
    "foo || bar" --> or [Var "foo", Var "bar"]
  describe "ternary operator" $ do
    specify "with primitive expressions" $
      "x ? 2 : 3" --> ternary [Var "x", num 2, num 3]
    specify "with parenthesized comparison" $
      "(1 > 0) ? 5 : -5" --> ternary [gt [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position" $
      ternaryIssue $ "1 > 0 ? 5 : -5" --> ternary [gt [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position, and addition in tail" $
      ternaryIssue $ "1 > 0 ? 5 : 1 + 2" -->
        ternary [gt [num 1, num 0], num 5, plus [num 1, num 2]]

literalSpec :: Spec
literalSpec = do
  it "handles integers" $
    "12356" -->  num 12356
  it "handles floats" $
    "23.42" -->  num 23.42
  describe "booleans" $ do
    it "accepts true" $ "true" --> bool True
    it "accepts false" $ "false" --> bool False

exprSpec :: Spec
exprSpec = do
  describe "literals" literalSpec
  describe "identifiers" $
    it "accepts valid variable names" $ do
      "foo" --> Var "foo"
      "foo_bar" --> Var "foo_bar"
  describe "grouping" $ do
    it "allows parens" $
      "( false )" -->  bool False
    it "handles vectors" $
      "[ 1, 2, 3 ]" -->  ListE [num 1, num 2, num 3]
    it "handles lists" $
      "( 1, 2, 3 )" -->  ListE [num 1, num 2, num 3]
    it "handles generators" $
      "[ a : 1 : b + 10 ]" -->
      fapp "list_gen" [Var "a", num 1, plus [Var "b", num 10]]
    it "handles indexing" $
      "foo[23]" --> index [Var "foo", num 23]
  describe "arithmetic" $ do
    it "handles unary +/-" $ do
      "-42" --> num (-42)
      "+42" -->  num 42
    it "handles +" $ do
      "1 + 2" --> plus [num 1, num 2]
      "1 + 2 + 3" --> plus [num 1, num 2, num 3]
    it "handles -" $ do
      "1 - 2" --> minus [num 1, num 2]
      "1 - 2 - 3" --> minus [minus [num 1, num 2], num 3]
    it "handles +/- in combination" $ do
      "1 + 2 - 3" --> plus [num 1, minus [num 2, num 3]]
      "2 - 3 + 4" --> plus [minus [num 2, num 3], num 4]
      "1 + 2 - 3 + 4" --> plus [num 1, minus [num 2, num 3], num 4]
      "1 + 2 - 3 + 4 - 5 - 6" --> plus [num 1,
                                           minus [num 2, num 3],
                                           minus [minus [num 4, num 5],
                                                     num 6]]
    it "handles exponentiation" $
      "x ^ y" -->  power [Var "x", Var "y"]
    it "handles *" $ do
      "3 * 4" -->  mult [num 3, num 4]
      "3 * 4 * 5" -->  mult [num 3, num 4, num 5]
    it "handles /" $
      "4.2 / 2.3" -->  divide [num 4.2, num 2.3]
    it "handles precedence" $
      "1 + 2 / 3 * 5" --> plus [num 1, mult [divide [num 2, num 3], num 5]]
    it "handles append" $
      "foo ++ bar ++ baz" --> append [Var "foo", Var "bar", Var "baz"]
  describe "logical operators" logicalSpec
  describe "application" $ do
    specify "base case" $ "foo(x)" --> Var "foo" :$ [Var "x"]
    specify "multiple arguments" $
      "foo(x, 1, 2)" --> Var "foo" :$ [Var "x", num 1, num 2]
    specify "multiple" $
      "foo(x, 1, 2)(5)(y)" --> ((Var "foo" :$ [Var "x", num 1, num 2]) :$ [num 5]) :$ [Var "y"]
    specify "multiple, with indexing" $
      "foo(x)[0](y)" --> ((index [(Var "foo" :$ [Var "x"]), num 0]) :$ [Var "y"])
