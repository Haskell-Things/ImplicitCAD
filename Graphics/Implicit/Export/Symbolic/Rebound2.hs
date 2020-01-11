-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2) where

import Graphics.Implicit.Definitions (BoxedObj2, ℝ2)

import Data.VectorSpace ((^-^), (^+^), (^/))

-- | Slightly stretch the bounding box of an object, in order to
--   ensure that during mesh generation, there are no problems because
--   values are right at the edge.
rebound2 :: BoxedObj2 -> BoxedObj2
rebound2 (obj, (a,b)) =
    let
        d :: ℝ2
        d = (b ^-^ a) ^/ 10
    in
        (obj, (a ^-^ d, b ^+^ d))
