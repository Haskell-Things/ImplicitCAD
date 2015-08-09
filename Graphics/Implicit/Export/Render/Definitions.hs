-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.Definitions where

import Graphics.Implicit.Definitions
import Control.DeepSeq

-- We want a format that can represent squares/quads and triangles.
-- So that we can merge squares and thereby reduces triangles.

-- Regarding Sq: Sq Basis@(b1,b2,b3) (Height on b3) 
--                  (b1 pos 1, b2 pos 1) (b1 pos 2, b2 pos 2)

data TriSquare = Sq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2 | Tris [Triangle]

-- For use with Parallel.Strategies later

instance NFData TriSquare where
    rnf (Sq b z xS yS) = rnf (b,z,xS,yS)
    rnf (Tris tris) = rnf tris

