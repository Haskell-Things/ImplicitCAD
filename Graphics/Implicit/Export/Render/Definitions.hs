-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- We want a type that can represent squares/quads and triangles.
module Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq)) where

import Prelude()

import Graphics.Implicit.Definitions(ℝ, ℝ2, ℝ3, TriangleMesh)

import Control.DeepSeq (NFData, rnf)

data TriSquare =
      Sq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2
    | Tris TriangleMesh

instance NFData TriSquare where
    rnf (Sq b z xS yS) = rnf (b,z,xS,yS)
    rnf (Tris tris) = rnf tris

