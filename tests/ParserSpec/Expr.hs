module ParserSpec.Expr (exprSpec) where

import Test.Hspec
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Expr
import Graphics.Implicit.ExtOpenScad.Parser.Statement
import ParserSpec.Util
import Text.ParserCombinators.Parsec hiding (State)
import Data.Either

infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  parse (expr0 <* eof) "<expr>" source `shouldBe` Right expr

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
    specify "single" $ "!foo" --> (app "!" [Var "foo"])
    specify "multiple" $
      negationIssue $ "!!!foo" --> app "!" [app "!" [app "!" [Var "foo"]]]
  it "handles and/or" $ do
    "foo && bar" --> app "&&" [Var "foo", Var "bar"]
    "foo || bar" --> app "||" [Var "foo", Var "bar"]
  describe "ternary operator" $ do
    specify "with primitive expressions" $
      "x ? 2 : 3" --> app "?" [Var "x", num 2, num 3]
    specify "with comparison in head position" $
      ternaryIssue $ "1 > 0 ? 5 : -5" --> app' "?" [app' ">" [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position, and addition in tail" $
      ternaryIssue $ "1 > 0 ? 5 : 1 + 2" -->
      app "?" [app ">" [num 1, num 0], num 5, plus [num 1, num 2]]

literalSpec :: Spec
literalSpec = do
  it "handles integers" $ do
    "12356" -->  num 12356
  it "handles floats" $ do
    "23.42" -->  num 23.42
  describe "booleans" $ do
    it "accepts true" $ "true" --> bool True
    it "accepts false" $ "false" --> bool False

exprSpec :: Spec
exprSpec = do
  describe "literals" literalSpec
  describe "identifiers" $ do
    it "accepts valid variable names" $ do
      "foo" --> Var "foo"
      "foo_bar" --> Var "foo_bar"
  describe "literals" $ literalSpec
  describe "grouping" $ do
    it "allows parens" $ do
      "( false )" -->  bool False
    it "handles vectors" $ do
      "[ 1, 2, 3 ]" -->  ListE [num 1, num 2, num 3]
    it "handles lists" $ do
      "( 1, 2, 3 )" -->  ListE [num 1, num 2, num 3]
    it "handles generators" $
      "[ a : 1 : b + 10 ]" -->
      (app' "list_gen" [Var "a", num 1, plus [Var "b", num 10]])
    it "handles indexing" $
      "foo[23]" --> Var "index" :$ [Var "foo", num 23]

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
      "1 + 2 - 3 + 4 - 5 - 6" --> plus [num 1, minus [num 2, num 3],
                                        minus [minus [num 4, num 5], num 6]]
    it "handles modulo" $
      "x % y" --> modulo [Var "x", Var "y"]
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
    "foo ++ bar ++ baz" --> app' "++" [Var "foo", Var "bar", Var "baz"]
  describe "logical operators" logicalSpec

  describe "application" $ do
    specify "base case" $ "foo(x)" --> Var "foo" :$ [Var "x"]
    specify "multiple arguments" $
      "foo(x, 1, 2)" --> Var "foo" :$ [Var "x", num 1, num 2]
    specify "multiple" $
      "foo(x, 1, 2)(5)(y)" --> ((Var "foo" :$ [Var "x", num 1, num 2])
                                :$ [num 5]) :$ [Var "y"]
    specify "multiple, with indexing" $
      "foo(x)[0](y)" --> ((app "index" [(Var "foo" :$ [Var "x"]), num 0])
                          :$ [Var "y"])
