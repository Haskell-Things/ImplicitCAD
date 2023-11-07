{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module ExecSpec.Expr (exprExec) where

-- Be explicit about what we import.
import Prelude (($), (==), length, null, Bool (False), (<=), (&&), (<>), show)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, shouldSatisfy, expectationFailure)

-- The type used for variables, in ImplicitCAD.
import Graphics.Implicit.Definitions (ℝ)

-- Our utility library, for making these tests easier to read.
import ExecSpec.Util ((-->), num, list, vect)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (runExpr)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal(OIO, OList, ONum, OUndefined))

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

exprExec :: Spec
exprExec = do
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
    it "performs matrix multiplication" $ do
      -- number - matrix, covered above but included for completness
      "4 * [[3, 4, -1], [0, 9, 5]]" --> list [vect [12, 16, -4], vect [0, 36, 20]]
      -- matrix - vector
      "[[1, -1, 2], [0, -3, 1]] * [2, 1, 0]" --> vect [1, -3]
      -- vector - matrix
      "[2, 1] * [[1, -1, 2], [0, -3, 1]]" --> vect [2, -5, 5]
      --matrix - matrix
      "[[12, 8, 4], [3, 17, 14], [9, 8, 10]] * [[5, 19, 3], [6, 15, 9], [7, 8, 16]]" --> list [vect [136, 380, 172], vect [215, 424, 386], vect [163, 371, 259]]
  describe "rands" $ do
    it "generates random numbers" $ do
      case runExpr "rands(1,2,1)" False of
        (OIO m, _) -> do
          OList l <- m
          shouldSatisfy l $ \l' -> length l' == 1
        _ -> expectationFailure "Not an OIO"
      case runExpr "rands(1,2,10)" False of
        (OIO m, _) -> do
          OList l <- m
          shouldSatisfy l $ \l' -> length l' == 10
        _ -> expectationFailure "Not an OIO"
      case runExpr "rands(1,2,0)" False of
        (OIO m, _) -> do
          OList l <- m
          shouldSatisfy l $ \l' -> null l'
        _ -> expectationFailure "Not an OIO"
      case runExpr "rands(1,1,1)" False of
        (OIO m, _) -> do
          OList l <- m
          shouldSatisfy l $ \l' ->
            length l' == 1 &&
            l' == [num 1]
        _ -> expectationFailure "Not an OIO"
      case runExpr "rands(1,2,1)[0]" False of
        (OIO m, _) -> do
          ONum n <- m
          shouldSatisfy n $ \n' -> 1 <= n' && n' <= 2
        o -> expectationFailure $ "Not an OIO: " <> show o
      case runExpr "rands(1,2,2)[0+1]" False of
        (OIO m, _) -> do
          ONum n <- m
          shouldSatisfy n $ \n' -> 1 <= n' && n' <= 2
        o -> expectationFailure $ "Not an OIO: " <> show o
  describe "lookup" $ do
    it "Gets a value from a table" $ do
      "lookup(1, [[0, 0], [1, 1], [2, 2]])" --> num 1
    it "Interpolates values from a table" $ do
      "lookup(1, [[0, 0], [2, 2]])" --> num 1
      "lookup(7, [[0, 0], [5, 50], [10, 100], [11, 0]])" --> num 70
      "lookup(10.5, [[0, 0], [5, 50], [10, 100], [11, 0]])" --> num 50
    it "Gets an upper extreme from a table" $ do
      "lookup(10, [[0, 0], [1, 1], [2, 2]])" --> num 2
    it "Gets an lower extreme from a table" $ do
      "lookup(0, [[1, 1], [2, 2]])" --> num 1
    it "Gets an nothing from a table" $ do
      "lookup(0, [])" --> OUndefined
    it "Handles embedded statements" $ do
      "lookup(0+1, [[0*2, 0], [1+1, 4/2]])" --> num 1
  describe "let bindings" $ do
    it "Evaluates let bindings" $ do
      -- basic let binding
      "let (a = 1) [a, 1]" --> vect [1, 1]
      -- Directly nested lets
      "let (a = 1) let (b = a) [a, b]" --> vect [1, 1]
      "let (a = 1) let (b = a) let (c = b) [a, b, c]" --> vect [1, 1, 1]
      "let (a = 1) let (b = a) let (c = a) [a, b, c]" --> vect [1, 1, 1]
      "let (a = 1) let (b = a) let (c = b + 1) [a, b, c]" --> vect [1, 1, 2]
      "let (a = 1) let (b = a) let (c = a + 1) [a, b, c]" --> vect [1, 1, 2]
      "let (a = 1) let (b = a+1) let (c = b+1) [a, b, c]" --> vect [1, 2, 3]
      "let (a = 1) let (a = a+1) [a]" --> vect [2]
      -- Indirect nesting
      "let (a = 1) [a, let (b = a) b]" --> vect [1, 1]
      -- Let name overloading
      "let (a = 1) let (b = a + 1) let (a = b) [a, a]" --> vect [2, 2]
      -- Scoped name overloading
      "let (a = 1) let (b = a + 1) [a, let (a = b) a]" --> vect [1, 2]
  describe "operator precedence" $ do
    -- https://github.com/Haskell-Things/ImplicitCAD/issues/428
    it "Evaluates exponents correctly" $ do
      "2*3^2" --> num 18
      "-2^2" --> num 4
      "-(2^2)" --> num (-4)
