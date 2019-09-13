-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

module ExecSpec.Expr (exprExec) where

-- Be explicit about what we import.
import Prelude (($))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it)

-- The type used for variables, in ImplicitCAD.
import Graphics.Implicit.Definitions (ℝ)

-- Our utility library, for making these tests easier to read.
import ExecSpec.Util ((-->), num)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

exprExec :: Spec
exprExec = do
  describe "arithmatic" $ do
    it "handles simple addition" $
      "1+1" --> num 2
    it "handles multiple addition" $
      "1+1+1" --> num 3
