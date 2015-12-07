-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.GetSegs where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.RefineSegs (refine)
import Graphics.Implicit.Export.Util (centroid)
import Data.VectorSpace

{- The goal of getSegs is to create polylines to separate 
   the interior and exterior vertices of a square intersectiong
   an object described by an implicit function.

      O.....O        O.....O
      :     :        :     :
      :     *        :  ,--*
      *     :   =>   *--   :
      :     :        :     :
      #.....#        #.....#

  An interior point is one at which obj is negative.
  
  What are all the variables?
  ===========================

  To allow data sharing, lots of values we 
  could calculate are instead arguments.


       positions               obj values
       ---------               ----------

  (x1,y2) .. (x2,y2)    obj   x1y2 .. x2y2
     :          :       =>     :       :
  (x1,y1) .. (x2,y1)          x1y1 .. x2y2


               mid points
               ----------

               (midy2V, y2)
                 = midy2

               ......*.....
               :          :
 (x1, midx1V)  *          *  (x2, midx2V)
   = midx1     :          :     = midx2
               :....*.....:

               (midy1V, y1)
                 = midy1

-}

getSegs :: ℝ2 -> ℝ2 -> Obj2 -> (ℝ,ℝ,ℝ,ℝ) -> (ℝ,ℝ,ℝ,ℝ) -> [Polyline]
{-- # INLINE getSegs #-}

getSegs p1 p2 obj (x1y1, x2y1, x1y2, x2y2) (midx1V,midx2V,midy1V,midy2V) = 
    let 
        (x,y) = p1

        -- Let's evaluate obj at a few points...
        c = obj (centroid [p1,p2])

        (dx,dy) = p2 ^-^ p1
        res = sqrt (dx*dy)

        midx1 = (x,      midx1V )
        midx2 = (x + dx, midx2V )
        midy1 = (midy1V , y )
        midy2 = (midy2V, y + dy)

        notPointLine (np1:np2:[]) = np1 /= np2

        -- takes straight lines between mid points and subdivides them to
        -- account for sharp corners, etc.

    in map (refine res obj) . filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
                                                            x1y1 <= 0, x2y1 <= 0) of

        -- An important point here is orientation. If you imagine going along a
        -- generated segment, the interior should be on the left-hand side.

        -- Empty Cases

        (True,  True, 
         True,  True)  -> []

        (False, False,
         False, False) -> []

        -- Horizontal Cases

        (True,  True, 
         False, False) -> [[midx1, midx2]]

        (False, False,
         True,  True)  -> [[midx2, midx1]]

        -- Vertical Cases

        (False, True, 
         False, True)  -> [[midy2, midy1]]

        (True,  False,
         True,  False) -> [[midy1, midy2]]

        -- Corner Cases

        (True,  False,
         False, False) -> [[midx1, midy2]]

        (False, True, 
         True,  True)  -> [[midy2, midx1]]

        (True,  True, 
         False, True)  -> [[midx1, midy1]]

        (False, False,
         True,  False) -> [[midy1, midx1]]

        (True,  True, 
         True,  False) -> [[midy1, midx2]]

        (False, False,
         False, True)  -> [[midx2, midy1]]

        (True,  False,
         True,  True)  -> [[midx2, midy2]]

        (False, True, 
         False, False) -> [[midy2, midx2]]

        -- Dual Corner Cases

        (True,  False,
         False, True)  -> if c <= 0
            then [[midx1, midy1], [midx2, midy2]]
            else [[midx1, midy2], [midx2, midy1]]

        (False, True, 
         True,  False) -> if c <= 0
            then [[midy2, midx1], [midy1, midx2]]
            else [[midy1, midx1], [midy2, midx2]]


-- A convenience function, we don't actually care too much about

{-- # INLINE getSegs' #-}

getSegs' (x1, y1) (x2, y2) obj (midx1V,midx2V,midy1V,midy2V) = 
    let
        x1y1 = obj (x1, y1)
        x2y1 = obj (x2, y1)
        x1y2 = obj (x1, y2)
        x2y2 = obj (x2, y2)
    in
        getSegs (x1, y1) (x2, y2) obj (x1y1, x2y1, x1y2, x2y2) (midx1V,midx2V,midy1V,midy2V)

