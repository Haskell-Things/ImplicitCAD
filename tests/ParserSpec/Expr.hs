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
  (parseExpr source) `shouldBe` Right expr

infixr 1 -->+
(-->+) :: String -> (Expr, String) -> Expectation
(-->+) source (result, leftover) =
  (parseWithLeftOver expr0 source) `shouldBe` (Right (result, leftover))

ternaryIssue :: Expectation -> Expectation
ternaryIssue _ = pendingWith "parser doesn't handle ternary operator correctly"

logicalSpec :: Spec
logicalSpec = do
  it "handles not" $ "!foo" --> (app' "!" [Var "foo"])
  it "handles and/or" $ do
    "foo && bar" --> app' "&&" [Var "foo", Var "bar"]
    "foo || bar" --> app' "||" [Var "foo", Var "bar"]
  describe "ternary operator" $ do
    specify "with primitive expressions" $
      "x ? 2 : 3" --> app' "?" [Var "x", num 2, num 3]
    specify "with comparison in head position" $
      ternaryIssue $ "1 > 0 ? 5 : -5" --> app' "?" [app' ">" [num 1, num 0], num 5, num (-5)]
    specify "with comparison in head position, and addition in tail" $
      ternaryIssue $ "1 > 0 ? 5 : 1 + 2" -->
      app' "?" [app' ">" [num 1, num 0], num 5, app "+" [num 1, num 2]]

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
      (app "list_gen" [Var "a", num 1, app "+" [Var "b", num 10]])
    it "handles indexing" $
      "foo[23]" --> Var "index" :$ [Var "foo", num 23]

  describe "arithmetic" $ do
    it "handles unary +/-" $ do
      "-42" --> num (-42)
      "+42" -->  num 42
    it "handles +" $ do
      "1 + 2" --> app "+" [num 1, num 2]
      "1 + 2 + 3" --> app "+" [num 1, num 2, num 3]
    it "handles -" $ do
      "1 - 2" --> app' "-" [num 1, num 2]
      "1 - 2 - 3" --> app' "-" [app' "-" [num 1, num 2], num 3]
    it "handles +/- in combination" $ do
      "1 + 2 - 3" --> app "+" [num 1, app' "-" [num 2, num 3]]
      "2 - 3 + 4" --> app "+" [app' "-" [num 2, num 3], num 4]
      "1 + 2 - 3 + 4" --> app "+" [num 1, app' "-" [num 2, num 3], num 4]
      "1 + 2 - 3 + 4 - 5 - 6" --> app "+" [num 1,
                                           app' "-" [num 2, num 3],
                                           app' "-" [app' "-" [num 4, num 5],
                                                     num 6]]
    it "handles exponentiation" $
      "x ^ y" -->  app' "^" [Var "x", Var "y"]
    it "handles *" $ do
      "3 * 4" -->  app "*" [num 3, num 4]
      "3 * 4 * 5" -->  app "*" [num 3, num 4, num 5]
    it "handles /" $
      "4.2 / 2.3" -->  app' "/" [num 4.2, num 2.3]
    it "handles precedence" $
      parseExpr "1 + 2 / 3 * 5" `shouldBe`
      (Right $ app "+" [num 1, app "*" [app' "/" [num 2, num 3], num 5]])
  it "handles append" $
    parseExpr "foo ++ bar ++ baz" `shouldBe`
    (Right $ app "++" [Var "foo", Var "bar", Var "baz"])
  describe "logical operators" logicalSpec
