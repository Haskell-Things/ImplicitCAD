-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.Expr (exprSpec) where

-- Be explicit about what we import.
import Prelude (Bool(True, False), ($))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, specify)

import Data.Text.Lazy (Text)

-- Parsed expression components.
import Graphics.Implicit.ExtOpenScad.Definitions (Expr(ListE, (:$)), Symbol(Symbol))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var), Pattern(Name))

-- The type used for variables, in ImplicitCAD.
import Graphics.Implicit.Definitions (ℝ)

-- Our utility library, for making these tests easier to read.
import ParserSpec.Util ((-->), fapp, num, bool, stringLiteral, undefined, plus, minus, mult, power, divide, negate, and, or, not, gt, ternary, append, index, lambda)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: Text -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: Text -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

logicalSpec :: Spec
logicalSpec = do
  describe "not" $ do
    specify "single" $ "!foo" --> not [Var "foo"]
    specify "double" $ "!!foo" --> Var "foo"
    specify "tripple" $ "!!!foo" --> not [Var "foo"]
  it "handles and/or" $ do
    "foo && bar" --> and [Var "foo", Var "bar"]
    "foo || bar" --> or [Var "foo", Var "bar"]
  describe "ternary operator" $ do
    specify "with primitive expressions" $
      "x ? 2 : 3" --> ternary [Var "x", num 2, num 3]
    specify "with parenthesized comparison" $
      "(1 > 0) ? 5 : -5" --> ternary [gt [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position" $
      "1 > 0 ? 5 : -5" --> ternary [gt [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position, and addition in tail" $
      "1 > 0 ? 5 : 1 + 2" -->
              ternary [gt [num 1, num 0], num 5, plus [num 1, num 2]]
    specify "nested in true and false expressions" $
      "c0 ? c1 ? t1 : f1 : c2 ? t2 : f2" -->
      ternary [Var "c0", ternary [Var "c1",Var "t1",Var "f1"], ternary [Var "c2",Var "t2",Var "f2"]]

literalSpec :: Spec
literalSpec = do
  it "handles integers" $
    "12356" --> num 12356
  it "handles positive leading zero integers" $
    "000012356" --> num 12356
  it "handles zero integer" $
    "0" --> num 0
  it "handles leading zero integer" $
    "0000" --> num 0
  it "handles floats" $
    "23.42" --> num 23.42
  it "handles floats with no whole component" $
    ".2342" --> num 0.2342
  describe "E notation" $ do
    it "accepts integer e with positive sign" $ "1e+1" --> num 10
    it "accepts integer e with negative sign" $ "10e-1" --> num 1
    it "accepts integer e with no sign" $ "1e1" --> num 10
    it "accepts floating e with positive sign" $ "1.1e+1" --> num 11
    it "accepts floating e with negative sign" $ "1.28e-1" --> num 0.128
    it "accepts floating e with no sign" $ "1.1e1" --> num 11
  describe "booleans" $ do
    it "accepts true" $ "true" --> bool True
    it "accepts false" $ "false" --> bool False
  describe "undefined" $
    it "accepts undef" $ "undef" --> undefined

letBindingSpec :: Spec
letBindingSpec = do
  it "handles let with integer binding and spaces" $
    "let ( a = 1 ) a" --> lambda [Name "a"] (Var "a") [num 1]
  it "handles multiple variable let" $
    "let (a = x, b = y) a + b" --> lambda [Name "a"] (lambda [Name "b"] (plus [Var "a", Var "b"]) [Var "y"]) [Var "x"]
  it "handles empty let" $
    "let () a" --> Var "a"
  it "handles nested let" $
    "let(a=x) let(b = y) a + b" --> lambda [Name "a"] (lambda [Name "b"] (plus [Var "a", Var "b"]) [Var "y"]) [Var "x"]
  it "handles let on right side of an arithmetic operator" $
    "1 + let(b = y) b" --> plus [num 1, lambda [Name "b"] (Var "b") [Var "y"]]
  it "handles let on right side of a unary negation" $
    "- let(b = y) b" --> negate [lambda [Name "b"] (Var "b") [Var "y"]]

exprSpec :: Spec
exprSpec = do
  describe "literals" literalSpec
  describe "identifiers" $
    it "accepts valid variable names" $ do
      "foo" --> Var "foo"
      "foo_bar" --> Var "foo_bar"
  describe "grouping" $ do
    it "allows parens" $
      "( false )" --> bool False
    it "handles empty vectors" $
      "[]" --> ListE []
    it "handles single element vectors" $
      "[a]" --> ListE [Var "a"]
    it "handles vectors" $
      "[ 1, 2, 3 ]" --> ListE [num 1, num 2, num 3]
    it "handles nested vectors" $
      "[ 1, [2, 7], [3, 4, 5, 6] ]" --> ListE [num 1, ListE [num 2, num 7], ListE [num 3, num 4, num 5, num 6]]
    it "handles lists" $
      "( 1, 2, 3 )" --> ListE [num 1, num 2, num 3]
    it "handles generators" $
      "[ a : b ]" -->
      fapp "list_gen" [Var "a", num 1, Var "b"]
    it "handles generators with expression" $
      "[ a : b + 10 ]" -->
      fapp "list_gen" [Var "a", num 1, plus [Var "b", num 10]]
    it "handles increment generators" $
      "[ a : 3 : b + 10 ]" -->
      fapp "list_gen" [Var "a", num 3, plus [Var "b", num 10]]
    it "handles indexing" $
      "foo[23]" --> index [Var "foo", num 23]
    it "handles multiple indexes" $
      "foo[23][12]" --> Var "index" :$ [Var "index" :$ [Var "foo", num 23], num 12]
    it "handles single function/module call with single argument" $
      "foo(1)" --> Var "foo" :$ [num 1]
    it "handles single function/module call with multiple arguments" $
      "foo(1, 2, 3)" --> Var "foo" :$ [num 1, num 2, num 3]
  describe "arithmetic" $ do
    it "handles unary -" $
      "-42" --> num (-42)
    it "handles unary +" $
      "+42" --> num 42
    it "handles unary - with extra spaces" $
      "-  42" --> num (-42)
    it "handles unary + with extra spaces" $
      "+  42" --> num 42
    it "handles unary - with parentheses" $
      "-(4 - 3)" --> negate [ minus [num 4, num 3]]
    it "handles unary + with parentheses" $
      "+(4 - 1)" --> minus [num 4, num 1]
    it "handles unary - with identifier" $
      "-foo" --> negate [Var "foo"]
    it "handles unary + with identifier" $
      "+foo" --> Var "foo"
    it "handles unary - with string literal" $
      "-\"foo\"" --> negate [stringLiteral "foo"]
    it "handles unary + with string literal" $
      "+\"foo\"" --> stringLiteral "foo"
    it "handles +" $ do
      "1 + 2" --> plus [num 1, num 2]
      "1 + 2 + 3" --> plus [plus [num 1, num 2], num 3]
    it "handles -" $ do
      "1 - 2" --> minus [num 1, num 2]
      "1 - 2 - 3" --> minus [minus [num 1, num 2], num 3]
    it "handles +/- in combination" $ do
      "1 + 2 - 3" --> minus [plus [num 1, num 2], num 3]
      "2 - 3 + 4" --> plus [minus [num 2, num 3], num 4]
      "1 + 2 - 3 + 4" --> plus [minus [ plus [num 1, num 2], num 3], num 4]
      "1 + 2 - 3 + 4 - 5 - 6" --> minus [minus [plus [minus [plus [num 1, num 2], num 3], num 4], num 5], num 6]
    it "handles exponentiation" $
      "x ^ y" --> power [Var "x", Var "y"]
    it "handles multiple exponentiations" $
      "x ^ y ^ z" --> power [Var "x", power [Var "y", Var "z"]]
    it "handles *" $
      "3 * 4" --> mult [num 3, num 4]
    it "handles > 2 term *" $
      "3 * 4 * 5" --> mult [mult [num 3, num 4], num 5]
    it "handles /" $
      "4.2 / 2.3" --> divide [num 4.2, num 2.3]
    it "handles precedence" $
      "1 + 2 / 3 * 5" --> plus [num 1, mult [divide [num 2, num 3], num 5]]
    it "handles append" $
      "foo ++ bar ++ baz" --> append [append [Var "foo", Var "bar"], Var "baz"]
  describe "logical operators" logicalSpec
  describe "let expressions" letBindingSpec
  describe "function/module application" $ do
    specify "base case" $ "foo(x)" --> Var "foo" :$ [Var "x"]
    specify "multiple arguments" $
      "foo(x, 1, 2)" --> Var "foo" :$ [Var "x", num 1, num 2]
