-- Implicit CAD. Copyright (C) 2012, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.HandlePolylines (cleanLoopsFromSegs) where

import Prelude(Maybe(Just, Nothing), fmap, (.), (==), last, reverse, ($), (<>), (-), (/), abs, (<=), (||), (&&), (*), (>), otherwise, error)

import Graphics.Implicit.Definitions (minℝ, Polyline(Polyline))

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
reducePolyline (Polyline ((x1,y1):(x2,y2):(x3,y3):others))
    -- Remove sequential duplicate points.
    | (x1,y1) == (x2,y2) = reducePolyline (Polyline ((x2,y2):(x3,y3):others))
    | abs ( (y2-y1)/(x2-x1) - (y3-y1)/(x3-x1) ) <= minℝ
      || ( (x2-x1) == 0 && (x3-x1) == 0 && (y2-y1)*(y3-y1) > 0) =
      reducePolyline (Polyline ((x1,y1):(x3,y3):others))
    | otherwise = Polyline ((x1,y1) : points (reducePolyline (Polyline ((x2,y2):(x3,y3):others))))
  where
    points (Polyline pts) = pts
-- | remove sequential duplicate points.
reducePolyline (Polyline ((x1,y1):(x2,y2):others)) =
    if (x1,y1) == (x2,y2) then reducePolyline (Polyline ((x2,y2):others)) else Polyline ((x1,y1):(x2,y2):others)
-- | Return the last result.
reducePolyline l@(Polyline ((_:_))) = l
-- Should not happen.
reducePolyline (Polyline []) = error "empty polyline"

{-cleanLoopsFromSegs =
    connectPolys
    -- . joinSegs
    . filter (not . degeneratePoly)

polylinesFromSegsOnGrid = undefined

degeneratePoly [] = True
degeneratePoly [a,b] = a == b
degeneratePoly _ = False

data SegOrPoly = Seg (ℝ2) ℝ ℝ2 -- Basis, shift, interval
               | Poly [ℝ2]

isSeg (Seg _ _ _) = True
isSeg _ = False

toSegOrPoly :: Polyline -> SegOrPoly
toSegOrPoly [a, b] = Seg v (a⋅vp) (a⋅v, b⋅v)
    where
        v@(va, vb) = normalized (b ^-^ a)
        vp = (-vb, va)
toSegOrPoly ps = Poly ps

fromSegOrPoly :: SegOrPoly -> Polyline
fromSegOrPoly (Seg v@(va,vb) s (a,b)) = [a*^v ^+^ t, b*^v ^+^ t]
    where t = s*^(-vb, va)
fromSegOrPoly (Poly ps) = ps

joinSegs :: [Polyline] -> [Polyline]
joinSegs = fmap fromSegOrPoly . joinSegs' . fmap toSegOrPoly

joinSegs' :: [SegOrPoly] -> [SegOrPoly]
joinSegs' segsOrPolys = polys <> (foldMap joinAligned aligned) where
    polys = filter (not.isSeg) segsOrPolys
    segs  = filter isSeg segsOrPolys
    aligned = groupWith (\(Seg basis p _) -> (basis,p)) segs

joinAligned segs@((Seg b z _):_) = mergeAdjacent orderedSegs where
    orderedSegs = sortBy (\(Seg _ _ (a1,_)) (Seg _ _ (b1,_)) -> compare a1 b1) segs
    mergeAdjacent (pres@(Seg _ _ (x1a,x2a)) : next@(Seg _ _ (x1b,x2b)) : others) =
        if x2a == x1b
        then mergeAdjacent ((Seg b z (x1a,x2b)): others)
        else pres : mergeAdjacent (next : others)
    mergeAdjacent a = a
joinAligned [] = []

connectPolys :: [Polyline] -> [Polyline]
connectPolys [] = []
connectPolys (present:remaining) =
    let
        findNext (ps@(p:_):segs) =
            if p == last present
            then (Just ps, segs)
            else (a, ps:b) where (a,b) =  findNext segs
        findNext [] = (Nothing, [])
    in
        case findNext remaining of
            (Nothing, _) -> present:(connectPolys remaining)
            (Just match, others) -> connectPolys $ (present <> tail match): others

-}
