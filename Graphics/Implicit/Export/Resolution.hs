-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.Implicit.Export.Resolution (estimateResolution) where

import Prelude (min, minimum, sqrt, ($), (*), (**), (-), (/), (>))
import Data.Maybe (Maybe (Just), fromMaybe)
import Graphics.Implicit (unionR)
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ)
import Graphics.Implicit.ExtOpenScad.Definitions (Message, OVal (ONum), VarLookup, lookupVarIn)
import Graphics.Implicit.Primitives (Object (getBox))
import Linear (V2 (V2), V3 (V3))
import Linear.Affine ((.-.))

-- | Find the resolution to raytrace at.
estimateResolution :: (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message]) -> ℝ
estimateResolution (lookupVarIn "$res" -> Just (ONum res), _, _, _) =
    -- If specified, use a resolution specified by the "$res" a variable in the input file.
    res
estimateResolution (vars, _, obj:objs, _) =
    -- If there was no resolution specified, use a resolution chosen for 3D objects.
    -- FIXME: magic numbers.
    let
        (V3 x1 y1 z1, V3 x2 y2 z2) = getBox (unionR 0 (obj:objs))
        (V3 x y z) = V3 (x2-x1) (y2-y1) (z2-z1)
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z)**(1/3) / 22)
estimateResolution (vars, obj:objs, _, _) =
    -- ... Or use a resolution chosen for 2D objects.
    --   FIXME: magic numbers.
    let
        (p1,p2) = getBox (unionR 0 (obj:objs))
        (V2 x y) = p2 .-. p1
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0 -> min (min x y/2) (sqrt(x*y/qual) / 30)
        _                    -> min (min x y/2) (sqrt(x*y) / 30)
estimateResolution _ =
    -- fallthrough value.
    1
