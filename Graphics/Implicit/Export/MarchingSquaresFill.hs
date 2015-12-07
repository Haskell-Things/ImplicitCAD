-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingSquaresFill (getContourMesh) where

import Graphics.Implicit.Definitions
-- FIXME: commented out, use later.
-- import Control.Parallel (par, pseq)

-- | getContour gets a polyline describe the edge of your 2D
--  object. It's really the only function in this file you need
--  to care about from an external perspective.

getContourMesh :: ℝ2 -> ℝ2 -> ℝ2 -> Obj2 -> [(ℝ2,ℝ2,ℝ2)]
getContourMesh (x1, y1) (x2, y2) (dx, dy) obj = 
    let
        -- How many steps will we take on each axis?
        nx = fromIntegral $ ceiling $ (x2 - x1) / dx
        ny = fromIntegral $ ceiling $ (y2 - y1) / dy
        -- Divide it up and compute the polylines
        trisOnGrid :: [[[(ℝ2,ℝ2,ℝ2)]]]
        trisOnGrid = [[getSquareTriangles
                   (x1 + (x2 - x1)*mx/nx,     y1 + (y2 - y1)*my/ny)
                   (x1 + (x2 - x1)*(mx+1)/nx, y1 + (y2 - y1)*(my+1)/ny)
                   obj
             | mx <- [0.. nx-1] ] | my <- [0..ny-1] ]
        triangles = concat $ concat trisOnGrid
    in
        triangles
        

-- | This function gives line segmensts to divde negative interior
--  regions and positive exterior ones inside a square, based on its 
--  values at its vertices.
--  It is based on the linearly-interpolated marching squares algorithm.

getSquareTriangles :: ℝ2 -> ℝ2 -> Obj2 -> [(ℝ2,ℝ2,ℝ2)]
getSquareTriangles (x1, y1) (x2, y2) obj = 
    let 
        (x,y) = (x1, y1)

        -- Let's evlauate obj at a few points...
        x1y1 = obj (x1, y1)
        x2y1 = obj (x2, y1)
        x1y2 = obj (x1, y2)
        x2y2 = obj (x2, y2)
        c = obj ((x1+x2)/2, (y1+y2)/2)

        dx = x2 - x1
        dy = y2 - y1

        -- linearly interpolated midpoints on the relevant axis
        --             midy2
        --      _________*__________
        --     |                    |
        --     |                    |
        --     |                    |
        --midx1*                    * midx2
        --     |                    |
        --     |                    |
        --     |                    |
        --     -----------*----------
        --              midy1

        midx1 = (x,                       y + dy*x1y1/(x1y1-x1y2))
        midx2 = (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
        midy1 = (x + dx*x1y1/(x1y1-x2y1), y )
        midy2 = (x + dx*x1y2/(x1y2-x2y2), y + dy)

        square aa bb cc dd = [(aa,bb,cc), (aa,cc,dd)]

    in case (x1y2 <= 0, x2y2 <= 0,
             x1y1 <= 0, x2y1 <= 0) of
        -- Yes, there's some symetries that could reduce the amount of code...
        -- But I don't think they're worth exploiting...
        (True,  True, 
         True,  True)  -> square (x1,y1) (x2,y1) (x2,y2) (x1,y2)
        (False, False,
         False, False) -> []
        (True,  True, 
         False, False) -> square midx1 midx2 (x2,y2) (x1,y2) 
        (False, False,
         True,  True)  -> square (x1,y1) (x2,y1) midx2 midx1 
        (False, True, 
         False, True)  -> square midy1 (x2,y1) (x2,y2) midy2
        (True,  False,
         True,  False) -> square (x1,y1) midy1 midy2 (x1,y2)
        (True,  False,
         False, False) -> [((x1,y2), midx1, midy2)]
        (False, True, 
         True,  True)  -> 
            [(midx1, (x1,y1), midy2), ((x1,y1), (x2,y1), midy2), (midy2, (x2,y1), (x2,y2))]
        (True,  True, 
         False, True)  -> 
            [((x1,y2), midx1, (x2,y2)), (midx1, midy1, (x2,y2)), ((x2,y2), midy1, (x2,y1))] 
        (False, False,
         True,  False) -> [(midx1, (x1,y1), midy1)]
        (True,  True, 
         True,  False) -> 
            [(midy1,midx2,(x2,y2)), ((x2,y2), (x1,y2), midy1), (midy1, (x1,y2), (x1,y1))]
        (False, False,
         False, True)  -> [(midx2, midy1, (x2,y1))]
        (True,  False,
         True,  True)  -> 
            [(midy2, (x2,y1), midx2), ((x2,y1), midy2, (x1,y1)), ((x1,y1), midy2, (x1,y2))]
        (False, True, 
         False, False) -> [(midx2, (x2,y2), midy2)]
        (True,  False,
         False, True)  -> if c > 0
            then [((x1,y2), midx1, midy2), ((x2,y1), midy1, midx2)]
            else [] --[[midx1, midy1], [midx2, midy2]]
        (False, True, 
         True,  False) -> if c <= 0
            then [] --[[midx1, midy2], [midx2, midy1]]
            else [((x1,y1), midy1, midx1), ((x2,y2), midx2, midy2)] --[[midx1, midy1], [midx2, midy2]]



