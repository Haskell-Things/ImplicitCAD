-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- We want a type that can represent squares/quads and triangles.
module Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq)) where

import Prelude (Show, Eq)

-- Points/Numbers, and the concept of an array of triangles.
import Graphics.Implicit.Definitions(ℝ, ℝ2, ℝ3, TriangleMesh)

-- So we can use Parallel on this type.
import Control.DeepSeq (NFData, rnf)

data TriSquare =
      Sq {
           _basis :: (ℝ3,ℝ3,ℝ3)
         , _zOffset :: ℝ
         , _xInterval :: ℝ2
         , _yInterval :: ℝ2
         , _origCoords :: (ℝ3,ℝ3,ℝ3,ℝ3)}
    | Tris TriangleMesh
  deriving (Show, Eq)

instance NFData TriSquare where
    rnf (Sq b z xS yS coords) = rnf (b,z,xS,yS,coords)
    rnf (Tris tris) = rnf tris

