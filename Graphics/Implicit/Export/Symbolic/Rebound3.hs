-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Symbolic.Rebound3 (rebound3) where

import Prelude()

import Graphics.Implicit.Definitions(BoxedObj3, ℝ3)

import Data.VectorSpace((^-^), (^+^), (^/))

rebound3 :: BoxedObj3 -> BoxedObj3
rebound3 (obj, (a,b)) =
    let
        d :: ℝ3
        d = (b ^-^ a) ^/ 10
    in
        (obj, ((a ^-^ d), (b ^+^ d)))

