{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module ExecSpec.Expr (exprExec) where

-- Be explicit about what we import.
import Prelude (($))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it)

-- The type used for variables, in ImplicitCAD.
import Graphics.Implicit.Definitions (ℝ)

-- Our utility library, for making these tests easier to read.
import ExecSpec.Util ((-->), num, list, vect)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

exprExec :: Spec
exprExec =
  describe "arithmetic" $ do
    it "performs simple addition" $
      "1+1" --> num 2
    it "performs multiple additions" $
      "1+1+1" --> num 3
    it "performs sum on numbers" $
      "sum(1,1)" --> num 2
    it "performs sum on list" $
      "sum([1,2,3])" --> num 6
    it "performs vector additions" $
      "[1, 2, 3] + [3, 4, 5]" --> vect [4, 6, 8]
    it "performs nested vector additions" $
      "[1, [2, 3]] + [3, [4, 5]]" --> list [num 4, vect [6, 8]]
    it "performs vector substraction" $
      "[1, 2, 3] - [1, 1, 1]" --> vect [0, 1, 2]
    it "performs number and list/vector addition" $
      "2 + [1, 2]" --> vect [3, 4]
    it "performs number and list/vector multiplication" $
      "2 * [3, 4, 5]" --> vect [6, 8, 10]
