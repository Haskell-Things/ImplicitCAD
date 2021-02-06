-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE

-- We want a type that can represent squares/quads and triangles.
module Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris, Sq), AnnotatedTriSquare(AnnotatedSq, AnnotatedTris)) where

import Prelude (seq)

-- Points/Numbers, and the concept of an array of triangles.
import Graphics.Implicit.Definitions(ℝ, ℝ2, ℝ3, TriangleMesh, AnnotatedTriangleMesh)

-- So we can use Parallel on this type.
import Control.DeepSeq (NFData, rnf)

data TriSquare =
      Sq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2
    | Tris TriangleMesh

data AnnotatedTriSquare a =
      AnnotatedSq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2 a
    | AnnotatedTris (AnnotatedTriangleMesh a)

instance NFData TriSquare where
    rnf (Sq b z xS yS) = rnf (b,z,xS,yS)
    rnf (Tris tris) = rnf tris

instance NFData a => NFData (AnnotatedTriSquare a) where
    rnf (AnnotatedSq b z xS yS a) = rnf (b,z,xS,yS) `seq` rnf a
    rnf (AnnotatedTris tris) = rnf tris
