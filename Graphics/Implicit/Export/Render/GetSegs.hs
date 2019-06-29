-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.GetSegs (getSegs) where

import Prelude(Bool(True, False), (+), (*), (/=), map, (.), filter, ($), (<=))

import Graphics.Implicit.Definitions (ℝ, ℝ2, Obj2, Polyline(Polyline), sqrt)

import Graphics.Implicit.Export.Render.RefineSegs (refine)

import Graphics.Implicit.Export.Util (centroid)

import Data.VectorSpace ((^-^))

{- The goal of getSegs is to create polylines to separate
   the interior and exterior vertices of a square intersecting
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
getSegs p1@(x,y) p2 obj (x1y1, x2y1, x1y2, x2y2) (midx1V,midx2V,midy1V,midy2V) =
    let
        -- Let's evaluate obj at a few points...
        c = obj (centroid [p1,p2])

        (dx,dy) = p2 ^-^ p1
        res = sqrt (dx*dy)

        midx1 = (x,      midx1V)
        midx2 = (x + dx, midx2V)
        midy1 = (midy1V, y     )
        midy2 = (midy2V, y + dy)

        notPointLine :: Polyline -> Bool
        notPointLine (Polyline [np1,np2]) = np1 /= np2
        notPointLine _ = False

        -- takes straight lines between mid points and subdivides them to
        -- account for sharp corners, etc.

    in map (refine res obj) . filter notPointLine $ case (x1y2 <= 0, x2y2 <= 0,
                                                          x1y1 <= 0, x2y1 <= 0) of

        -- An important point here is orientation. If you imagine going along a
        -- generated segment, the interior should be on the left-hand side.

        -- Empty Cases

        (True,   True,  True,  True) -> []
        (False, False, False, False) -> []

        -- Horizontal Cases
        ( True,  True, False, False) -> [Polyline [midx1, midx2]]
        (False, False,  True,  True) -> [Polyline [midx2, midx1]]

        -- Vertical Cases
        (False,  True, False,  True) -> [Polyline [midy2, midy1]]
        ( True, False,  True, False) -> [Polyline [midy1, midy2]]

        -- Corner Cases
        ( True, False, False, False) -> [Polyline [midx1, midy2]]
        (False,  True,  True,  True) -> [Polyline [midy2, midx1]]
        ( True,  True, False,  True) -> [Polyline [midx1, midy1]]
        (False, False,  True, False) -> [Polyline [midy1, midx1]]
        ( True,  True,  True, False) -> [Polyline [midy1, midx2]]
        (False, False, False,  True) -> [Polyline [midx2, midy1]]
        ( True, False,  True,  True) -> [Polyline [midx2, midy2]]
        (False,  True, False, False) -> [Polyline [midy2, midx2]]

        -- Dual Corner Cases
        (True,  False, False, True)  -> if c <= 0
            then [Polyline [midx1, midy1], Polyline [midx2, midy2]]
            else [Polyline [midx1, midy2], Polyline [midx2, midy1]]

        (False, True, True,  False) -> if c <= 0
            then [Polyline [midy2, midx1], Polyline [midy1, midx2]]
            else [Polyline [midy1, midx1], Polyline [midy2, midx2]]
