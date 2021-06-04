{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2012, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.HandlePolylines (cleanLoopsFromSegs) where

import Prelude(Maybe(Just, Nothing), fmap, (.), (==), last, reverse, ($), (<>), (-), (/), abs, (<=), (||), (&&), (*), (>), otherwise, error)

import Graphics.Implicit.Definitions (minℝ, Polyline(Polyline))
import Linear ( V2(V2) )

cleanLoopsFromSegs :: [Polyline] -> [Polyline]
cleanLoopsFromSegs = fmap reducePolyline . joinSegs

-- | Join polylines that connect.
joinSegs :: [Polyline] -> [Polyline]
joinSegs (Polyline present:remaining) =
    let
        findNext :: [Polyline] -> (Maybe Polyline, [Polyline])
        findNext (Polyline (p3:ps):segs)
            | p3 == last present      = (Just (Polyline (p3:ps)), segs)
            | last ps == last present = (Just (Polyline $ reverse $ p3:ps), segs)
            | otherwise               = case findNext segs of (res1,res2) -> (res1,Polyline (p3:ps):res2)
        findNext [] = (Nothing, [])
        findNext (Polyline []:_) = (Nothing, [])
    in
        case findNext remaining of
            (Nothing, _) -> Polyline present: joinSegs remaining
            (Just (Polyline match), others) -> joinSegs $ Polyline (present <> match) : others
joinSegs [] = []

-- | Simplify and sort a polyline.
reducePolyline :: Polyline -> Polyline
reducePolyline (Polyline (V2 x1 y1 : V2 x2 y2 : V2 x3 y3:others))
    -- Remove sequential duplicate points.
    | (x1,y1) == (x2,y2) = reducePolyline (Polyline (V2 x2 y2 : V2 x3 y3 : others))
    | abs ( (y2-y1)/(x2-x1) - (y3-y1)/(x3-x1) ) <= minℝ
      || ( (x2-x1) == 0 && (x3-x1) == 0 && (y2-y1)*(y3-y1) > 0) =
      reducePolyline (Polyline (V2 x1 y1 : V2 x3 y3 :others))
    | otherwise = Polyline (V2 x1 y1 : points (reducePolyline (Polyline (V2 x2 y2 : V2 x3 y3 : others))))
  where
    points (Polyline pts) = pts
-- | remove sequential duplicate points.
reducePolyline (Polyline (V2 x1 y1 : V2 x2 y2 : others)) =
    if (x1,y1) == (x2,y2) then reducePolyline (Polyline (V2 x2 y2 : others)) else Polyline (V2 x1 y1 : V2 x2 y2 : others)
-- Return the last result.
reducePolyline l@(Polyline ((_:_))) = l
-- Should not happen.
reducePolyline (Polyline []) = error "empty polyline"

