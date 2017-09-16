import Test.Hspec
import ParserSpec.Statement
import ParserSpec.Expr

main :: IO ()
main = hspec $ do
  describe "expressions" exprSpec
  describe "statements" statementSpec
