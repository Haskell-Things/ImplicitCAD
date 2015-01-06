-- Implicit CAD. Copyright (C) 2012, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.HandlePolylines (cleanLoopsFromSegs) where

import Graphics.Implicit.Definitions

cleanLoopsFromSegs :: [Polyline] -> [Polyline]
cleanLoopsFromSegs =
    map reducePolyline
    . joinSegs
    . filter polylineNotNull


joinSegs :: [Polyline] -> [Polyline]
joinSegs [] = []
joinSegs (present:remaining) =
    let
        findNext ((p3:ps):segs) = if p3 == last present then (Just (p3:ps), segs) else
            if last ps == last present then (Just (reverse $ p3:ps), segs) else
            case findNext segs of (res1,res2) -> (res1,(p3:ps):res2)
        findNext [] = (Nothing, [])
    in
        case findNext remaining of
            (Nothing, _) -> present:(joinSegs remaining)
            (Just match, others) -> joinSegs $ (present ++ tail match): others

reducePolyline ((x1,y1):(x2,y2):(x3,y3):others) = 
    if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):(x3,y3):others) else
    if abs ( (y2-y1)/(x2-x1) - (y3-y1)/(x3-x1) ) < 0.0001 
       || ( (x2-x1) == 0 && (x3-x1) == 0 && (y2-y1)*(y3-y1) > 0)
    then reducePolyline ((x1,y1):(x3,y3):others)
    else (x1,y1) : reducePolyline ((x2,y2):(x3,y3):others)
reducePolyline ((x1,y1):(x2,y2):others) = 
    if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):others) else (x1,y1):(x2,y2):others
reducePolyline l = l

polylineNotNull (_:l) = not (null l)
polylineNotNull [] = False



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
joinSegs = map fromSegOrPoly . joinSegs' . map toSegOrPoly

joinSegs' :: [SegOrPoly] -> [SegOrPoly]
joinSegs' segsOrPolys = polys ++ concat (map joinAligned aligned) where
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
            (Just match, others) -> connectPolys $ (present ++ tail match): others

-}
