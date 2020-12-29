-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2018, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- be explicit about what we import.
import Prelude (($), IO)

-- our testing engine.
import Test.Hspec(hspec, describe)

-- the parser test for statements.
import ParserSpec.Statement(statementSpec)

-- the parser test for expressions.
import ParserSpec.Expr(exprSpec)

-- the execution test for expressions.
import ExecSpec.Expr(exprExec)

-- the execution test for warnings.
import MessageSpec.Message(programExec)

import PropertySpec (propSpec)

import qualified GoldenSpec.Spec as Golden
import qualified ImplicitSpec as Implicit
import qualified TesselationSpec as Tesselation
import qualified VectorV3Spec as VectorV3

main :: IO ()
main = hspec $ do
  -- run the golden tests to ensure we haven't broken mesh generation
  describe "golden tests" Golden.spec
  -- run tests against the expression parsing engine.
  describe "expression parsing" exprSpec
  -- and now, against the statement parsing engine.
  describe "statements parsing" statementSpec
  -- run tests against the expression execution engine. single statements.
  describe "expression execution" exprExec
  -- run tests against the evaluation engine, checking for messages.
  describe "program execution" programExec

  -- Generate data to be evaluated, and ensure the properties hold.
  describe "property tests" propSpec

  Implicit.spec
  Tesselation.spec
  VectorV3.spec

