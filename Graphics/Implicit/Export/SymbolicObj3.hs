-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- The purpose of this function is to symbolicaly compute triangle meshes where possible.
-- Otherwise we coerce it into an implicit function and apply our modified marching cubes algorithm.

-- We just want to export the instance...
module Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh) where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.Render (getMesh)

import Graphics.Implicit.ObjectUtil
import Graphics.Implicit.MathUtil

import Graphics.Implicit.Export.Symbolic.Rebound3
import Graphics.Implicit.Export.Util (normTriangle)


instance DiscreteAproxable SymbolicObj3 TriangleMesh where
    discreteAprox res obj = symbolicGetMesh res obj

instance DiscreteAproxable SymbolicObj3 NormedTriangleMesh where
    discreteAprox res obj = map (normTriangle res (getImplicit3 obj)) $ symbolicGetMesh res obj

symbolicGetMesh :: ℝ -> SymbolicObj3 -> [(ℝ3, ℝ3, ℝ3)]

{--
-- A translated objects mesh is its mesh translated.
symbolicGetMesh res (Translate3 v obj) = 
    map (\(a,b,c) -> (a S.+ v, b S.+ v, c S.+ v) ) (symbolicGetMesh res obj)

-- A scaled objects mesh is its mesh scaled
symbolicGetMesh res (Scale3 s obj) =
    let
        mesh :: [(ℝ3, ℝ3, ℝ3)]
        mesh = symbolicGetMesh res obj
        scaleTriangle :: (ℝ3, ℝ3, ℝ3) -> (ℝ3, ℝ3, ℝ3)
        scaleTriangle (a,b,c) = (s S.⋯* a, s S.⋯* b, s S.⋯* c)
    in map scaleTriangle  mesh

-- A couple triangles make a cube...
symbolicGetMesh _ (Rect3R 0 (x1,y1,z1) (x2,y2,z2)) = 
    let
        square a b c d = [(a,b,c),(d,a,c)]
        rsquare a b c d = [(c,b,a),(c,a,d)]
    in
           rsquare (x1,y1,z1) (x2,y1,z1) (x2,y2,z1) (x1,y2,z1)
        ++ square (x1,y1,z2) (x2,y1,z2) (x2,y2,z2) (x1,y2,z2)
        ++ square (x1,y1,z1) (x2,y1,z1) (x2,y1,z2) (x1,y1,z2)
        ++ rsquare (x1,y2,z1) (x2,y2,z1) (x2,y2,z2) (x1,y2,z2)
        ++ square (x1,y1,z1) (x1,y1,z2) (x1,y2,z2) (x1,y2,z1)
        ++ rsquare (x2,y1,z1) (x2,y1,z2) (x2,y2,z2) (x2,y2,z1)

-- Use spherical coordinates to create an easy tesselation of a sphere
symbolicGetMesh res (Sphere r) = half1 ++ half2
    where
        -- Convenience functions for mesh generation
        square a b c d = [(a,b,c),(d,a,c)]
        rsquare a b c d = [(c,b,a),(c,a,d)]
        -- Number of steps of φ and θ respectivly
        m = max 3 (fromIntegral $ ceiling $ 1.5*r/res)
        n = 2*m
        -- Spherical coordinates
        spherical θ φ = (r*cos(θ), r*sin(θ)*cos(φ), r*sin(θ)*sin(φ))
        -- Function placing steps on sphere
        f n' m' = spherical (2*pi*n'/n) (pi*m'/m)
        -- Mesh in two pieces..
        half1 = concat [ square (f m1 m2) (f (m1+1) m2) (f (m1+1) (m2+1)) (f m1 (m2+1)) 
                        | m1 <- [0.. m-1], m2 <- [0.. m-1] ]
        half2 = concat [ rsquare (f m1 m2) (f (m1+1) m2) (f (m1+1) (m2+1)) (f m1 (m2+1)) 
                        | m1 <- [m.. n-1], m2 <- [0.. m-1] ]

{-symbolicGetMesh res (UnionR3 r [ExtrudeR ra obja ha, ExtrudeR rb objb hb]) 
    | ha == hb && ra == rb = symbolicGetMesh res $ ExtrudeR ra (UnionR2 r [obja, objb]) ha

symbolicGetMesh res (UnionR3 r [ExtrudeR ra obja ha, ExtrudeR rb objb hb, ExtrudeR rc objc hc]) 
    | ha == hb && ha == hc && ra == rb && ra == rc = 
        symbolicGetMesh res $ ExtrudeR ra (UnionR2 r [obja, objb, objc]) ha-}

-- We can compute a mesh of a rounded, extruded object from it contour, 
-- contour filling trinagles, and magic.
-- General approach:
--   - generate sides by basically cross producting the contour.
--   - generate the the top by taking the contour fill and
--     calculating an appropriate z height.
symbolicGetMesh res  (ExtrudeR r obj2 h) = 
    let
        -- Get a Obj2 (magnitude descriptor object)
        obj2mag :: ℝ2 -> ℝ -- Obj2
        obj2mag = getImplicit2 obj2
        -- The amount that a point (x,y) on the top should be lifted
        -- from h-r. Because of rounding, the edges should be h-r,
        -- but it should increase inwards.
        dh x y = sqrt (r^2 - ( max 0 $ min r $ r+obj2mag (x,y))^2)
        -- Turn a polyline into a list of its segments
        segify (a:b:xs) = (a,b):(segify $ b:xs)
        segify _ = []
        -- Flip a triangle. It's the same triangle with opposite handedness.
        flipTri (a,b,c) = (a,c,b)
        -- Turn a segment a--b into a list of triangles forming (a--b)×(r,h-r)
        -- The dh stuff is to compensate for rounding errors, etc, and ensure that
        -- the sides meet the top and bottom
        segToSide (x1,y1) (x2,y2) =
            [((x1,y1,r-dh x1 y1), (x2,y2,r-dh x2 y2), (x2,y2,h-r+dh x2 y2)), 
             ((x1,y1,r-dh x1 y1), (x2,y2,h-r+dh x2 y2), (x1,y1,h-r+dh x1 y1)) ]
        -- Get a contour polyline for obj2, turn it into a list of segments
        segs = concat $ map segify $ symbolicGetOrientedContour res obj2
        -- Create sides for the main body of our object = segs × (r,h-r)
        side_tris = concat $ map (\(a,b) -> segToSide a b) segs
        -- Triangles that fill the contour. Make sure the mesh is at least (res/5) fine.
        -- --res/5 because xyres won't always match up with normal res and we need to compensate.
        fill_tris = {-divideMeshTo (res/5) $-} symbolicGetContourMesh res obj2
        -- The bottom. Use dh to determine the z coordinates
        bottom_tris = map flipTri $ [((a1,a2,r-dh a1 a2), (b1,b2,r - dh b1 b2), (c1,c2,r - dh c1 c2)) 
                | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
        -- Same idea at the top.
        top_tris = [((a1,a2,h-r+dh a1 a2), (b1,b2,h-r+dh b1 b2), (c1,c2,h-r+dh c1 c2)) 
                | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
    in
        -- Merge them all together! :)
        side_tris ++ bottom_tris ++ top_tris 


symbolicGetMesh res  (ExtrudeRM r twist scale translate obj2 h) = 
    let
        -- Get a Obj2 (magnitude descriptor object)
        obj2mag :: Obj2 -- = ℝ2 -> ℝ
        obj2mag = getImplicit2 obj2
        -- cleanup twist, scale, etc
        twist' = Maybe.fromMaybe (const 0) twist
        scale' = Maybe.fromMaybe (const 1) scale
        translate' = Maybe.fromMaybe (const (0,0)) translate
        h' = case h of
            Left n -> const n
            Right f -> f
        -- The amount that a point (x,y) on the top should be lifted
        -- from h-r. Because of rounding, the edges should be h-r,
        -- but it should increase inwards.
        dh x y = sqrt (r^2 - ( max 0 $ min r $ r+obj2mag (x,y))^2)
        -- Turn a polyline into a list of its segments
        segify (a:b:xs) = (a,b):(segify $ b:xs)
        segify _ = []
        -- Flip a triangle. It's the same triangle with opposite handedness.
        flipTri (a,b,c) = (a,c,b)
        -- The number of steps we're going to do the sides in:
        n = max 4 $ fromIntegral $ ceiling $ h' (0,0)/res
        -- Turn a segment a--b into a list of triangles forming 
        --    (a--b)×(r+(h-2r)*m/n,r+(h-2r)*(m+1)/n)
        -- The dh stuff is to compensate for rounding errors, etc, and ensure that
        -- the sides meet the top and bottom
        -- m is the number of n steps we are up from the base of the main section
        segToSide m (x1,y1) (x2,y2) =
            let
                -- Change across the main body of the object,
                -- at (x1,y1) and (x2,y2) respectivly
                mainH1 = h' (x1, y1) - 2*r + 2*dh x1 y1
                mainH2 = h' (x2, y2) - 2*r + 2*dh x2 y2
                -- level a (lower) and level b (upper)
                la1 = r-dh x1 y1  +  mainH1*m/n
                lb1 = r-dh x1 y1  +  mainH1*(m+1)/n
                la2 = r-dh x2 y2  +  mainH2*m/n
                lb2 = r-dh x2 y2  +  mainH2*(m+1)/n
            in
                -- Resulting triangles: 
                [((x1,y1,la1), (x2,y2,la2), (x2,y2,lb2)), 
                 ((x1,y1,la1), (x2,y2,lb2), (x1,y1,lb1)) ]
        -- Get a contour polyline for obj2, turn it into a list of segments
        segs = concat $ map segify $ symbolicGetOrientedContour res obj2
        -- Create sides for the main body of our object = segs × (r,h-r)
        -- Many layers...
        side_tris = map flipTri $ concat $
            [concat $ map (\(a,b) -> segToSide m a b) segs | m <- [0.. n-1] ]
        -- Triangles that fill the contour. Make sure the mesh is at least (res/5) fine.
        -- --res/5 because xyres won't always match up with normal res and we need to compensate.
        fill_tris = {-divideMeshTo (res/5) $-} symbolicGetContourMesh res obj2
        -- The bottom. Use dh to determine the z coordinates
        bottom_tris = [((a1,a2,r-dh a1 a2), (b1,b2,r - dh b1 b2), (c1,c2,r - dh c1 c2)) 
                | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
        -- Same idea at the top.
        top_tris = map flipTri $ [((a1,a2,h' (a1,a2) -r+dh a1 a2), (b1,b2,h' (b1,b2) -r+dh b1 b2), (c1,c2,h' (c1,c2)-r+dh c1 c2)) 
                | ((a1,a2),(b1,b2),(c1,c2)) <- fill_tris]
        -- Mesh modifiers in individual components
        k = 2*pi/360
        fx :: ℝ3 -> ℝ
        fx (x,y,z) = let (tx,ty) = translate' z in
            scale' z *((x+tx)*cos(k*twist' z) + (y+ty)*sin(k*twist' z))
        fy :: ℝ3 -> ℝ
        fy (x,y,z) =let (tx,ty) = translate' z in
            scale' z *((x+tx)*sin(k*twist' z) - (y+ty)*cos(k*twist' z))
        -- function to transform a triangle
        transformTriangle :: (ℝ3,ℝ3,ℝ3) -> (ℝ3,ℝ3,ℝ3)
        transformTriangle (a@(_,_,z1), b@(_,_,z2), c@(_,_,z3)) = 
            ((fx a, fy a, z1), (fx b, fy b, z2), (fx c, fy c, z3))

    in
        map transformTriangle (side_tris ++ bottom_tris ++ top_tris)
-}

symbolicGetMesh res inputObj@(UnionR3 r objs) = 
    let
        boxes = map getBox3 objs
        boxedObjs = zip boxes objs
        
        sepFree ((box,obj):others) = 
            if length (filter (box3sWithin r box) boxes) > 1
            then (\(a,b) -> (obj:a,b)) $ sepFree others
            else (\(a,b) -> (a,obj:b)) $ sepFree others
        sepFree [] = ([],[])

        (dependants, independents) = sepFree boxedObjs
    in if null independents
    then case rebound3 (getImplicit3 inputObj, getBox3 inputObj) of
        (obj, (a,b)) -> getMesh a b res obj 
    else if null dependants
    then concat $ map (symbolicGetMesh res) independents
    else concat $ 
        map (symbolicGetMesh res) independents 
        ++ [symbolicGetMesh res (UnionR3 r dependants)]

-- If all that fails, coerce and apply marching cubes :(
-- (rebound is for being safe about the bounding box --
--  it slightly streches it to make sure nothing will 
--  have problems because it is right at the edge )
symbolicGetMesh res obj =
    case rebound3 (getImplicit3 obj, getBox3 obj) of
        (obj, (a,b)) -> getMesh a b res obj 

