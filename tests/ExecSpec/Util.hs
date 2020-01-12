-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Utilities
module ExecSpec.Util
       ( (-->)
       , num
       ) where

-- be explicit about where we get things from.
import Prelude (String)

-- The datatype of positions in our world.
import Graphics.Implicit.Definitions (ℝ)

-- Expressions, symbols, and values in the OpenScad language.
import Graphics.Implicit.ExtOpenScad.Definitions (OVal(ONum))


import Graphics.Implicit.ExtOpenScad.Eval.Constant (runExpr)

import Test.Hspec (Expectation, shouldBe)

-- An operator for expressions for "the left side should evaluate to the right side."
infixr 1 -->
(-->) :: String -> OVal -> Expectation
(-->) source value =
  runExpr source `shouldBe` (value, [])

-- | Types

num :: ℝ -> OVal
num = ONum
