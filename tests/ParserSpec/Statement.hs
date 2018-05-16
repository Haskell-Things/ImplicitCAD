module ParserSpec.Statement (statementSpec) where

import Test.Hspec
import Text.ParserCombinators.Parsec hiding (State)
import ParserSpec.Util
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Statement
import Data.Either

parsesAs :: String -> [StatementI] -> Expectation
parsesAs source stmts =
  (parseProgram "src" source) `shouldBe` Right stmts

parsesAsError :: String -> Expectation
parsesAsError source =
  (parseProgram "src" source) `shouldSatisfy` isLeft

single :: Statement StatementI -> [StatementI]
single st = [StatementI 1 st]

call :: Symbol -> [(Maybe Symbol, Expr)] -> [StatementI] -> StatementI
call name args stmts = StatementI 1 (ModuleCall name args stmts)

ifSpec :: Spec
ifSpec = do
  it "parses" $
    "if (true) { a(); } else { b(); }" `parsesAs` (
      single $ If (bool True) [call "a" [] []] [call "b" [] []])

assignmentSpec :: Spec
assignmentSpec = do
  it "parses correctly" $
    "y = -5;" `parsesAs` (single $ Name "y" := (num (-5)))
  it "handles pattern matching" $
    "[x, y] = [1, 2];" `parsesAs`
    (single $ ListP [Name "x", Name "y"] := (ListE [num 1, num 2]))
  it "handles the function keyword and definitions" $
    "function foo(x, y) = x * y;" `parsesAs` single fooFunction
  it "nested indexing" $
    "x = [y[0] - z * 2];" `parsesAs`
    (single $ Name "x" := ListE [app' "-" [app' "index" [Var "y", num 0],
                                           app "*" [Var "z", num 2]]])
  where
    fooFunction = Name "foo" := (LamE [Name "x", Name "y"]
                                 (app "*" [Var "x", Var "y"]))

emptyFileIssue :: Expectation -> Expectation
emptyFileIssue _ = pendingWith "parser should probably allow empty files"

statementSpec :: Spec
statementSpec = do
  describe "empty file" $ do
    it "returns an empty list" $
      emptyFileIssue $ "" `parsesAs` []

  describe "line comment" $ do
    it "parses as empty" $ emptyFileIssue $ "// foish bar\n" `parsesAs` []

  describe "module call" $ do
    it "parses" $
      "foo();" `parsesAs` (single $ ModuleCall "foo" [] [])
  describe "difference of two cylinders" $ do
    it "parses correctly" $
      "difference(){ cylinder(r=5,h=20); cylinder(r=2,h=20); }"
      `parsesAs` single (
        ModuleCall "difference" [] [
           (call "cylinder" [(Just "r", num 5.0),
                             (Just "h", num 20.0)]
            []),
           (call "cylinder" [(Just "r", num 2.0),
                             (Just "h", num 20.0)]
            [])])

  describe "empty module definition" $ do
    it "parses correctly" $
      "module foo_bar() {}" `parsesAs` (single $ NewModule "foo_bar" [] [])

  describe "assignment" assignmentSpec

  describe "if" ifSpec
