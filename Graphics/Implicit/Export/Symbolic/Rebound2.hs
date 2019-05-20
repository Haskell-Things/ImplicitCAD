-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2) where

import Prelude()

import Graphics.Implicit.Definitions (BoxedObj2, ℝ, ℝ2)

import Data.VectorSpace ((^-^), (^+^), (^/))

default (ℝ)

rebound2 :: BoxedObj2 -> BoxedObj2
rebound2 (obj, (a,b)) =
    let
        d :: ℝ2
        d = (b ^-^ a) ^/ 10
    in
        (obj, (a ^-^ d, b ^+^ d))
