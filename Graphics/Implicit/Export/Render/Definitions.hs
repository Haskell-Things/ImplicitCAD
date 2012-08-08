-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.Definitions where

import Graphics.Implicit.Definitions
import Control.DeepSeq

data TriSquare = Sq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2 | Tris [Triangle]

instance NFData TriSquare where
	rnf (Sq b z xS yS) = rnf (b,z,xS,yS)
	rnf (Tris tris) = rnf tris

