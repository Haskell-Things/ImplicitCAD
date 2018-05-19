-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2018, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- be explicit about what we import.
import Prelude (($), IO)

-- our testing engine.
import Test.Hspec(hspec, describe)

-- the test forstatements.
import ParserSpec.Statement(statementSpec)

-- the test for expressions.
import ParserSpec.Expr(exprSpec)

main :: IO ()
main = hspec $ do
  -- run tests against the expression engine.
  describe "expressions" exprSpec
  -- and now, against the statement engine.
  describe "statements" statementSpec
