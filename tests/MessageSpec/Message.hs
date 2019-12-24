-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

module MessageSpec.Message (programExec) where

-- Be explicit about what we import.
import Prelude (($))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it)

-- The types used for variables, in ImplicitCAD.
import Graphics.Implicit.Definitions (Fastℕ, ℝ)

import Graphics.Implicit.ExtOpenScad.Definitions (MessageType(TextOut), SourcePosition(SourcePosition))

-- Our utility library, for making these tests easier to read.
import MessageSpec.Util ((-->), oneMessage)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (Fastℕ, ℝ)

programExec :: Spec
programExec =
  describe "arithmatic" $ do
    it "echoes simple addition" $
      "echo(1+1);" --> oneMessage TextOut (SourcePosition 1 1 []) "2.0"
    it "calls a no argument function" $
      "module a(){echo(1);}a();" --> oneMessage TextOut (SourcePosition 1 12 []) "1.0"
