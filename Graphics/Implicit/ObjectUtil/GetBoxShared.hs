-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Graphics.Implicit.ObjectUtil.GetBoxShared where

import Prelude
import {-# SOURCE #-} Graphics.Implicit.Primitives
    ( Object(getBox) )
import Graphics.Implicit.Definitions
    ( SharedObj(..), ComponentWiseMultable((⋯*)), ℝ3, ℝ2, ℝ )
import Data.VectorSpace
    ( AdditiveGroup((^+^), zeroV, (^-^)),
      InnerSpace,
      VectorSpace(Scalar) )
import Graphics.Implicit.MathUtil ( reflect )
import qualified Linear.Quaternion as Q
import Data.Semigroup ( Semigroup(sconcat), Max(Max), Min(Min) )
import Control.Arrow ((***),  Arrow((&&&)) )
import Data.Semigroup ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Coerce ( coerce )
import Data.Bifunctor (Bifunctor(bimap))


class VectorStuff vec where
  uniformV :: ℝ -> vec
  corners :: (vec, vec) -> [vec]
  pointwise :: (ℝ -> ℝ -> ℝ) -> vec -> vec -> vec
  elements :: vec -> [ℝ]

instance VectorStuff ℝ2 where
  uniformV x = (x, x)
  corners (p1@(x1, y1), p2@(x2, y2)) =
    [ p1
    , (x1, y2)
    , (x2, y1)
    , p2
    ]
  pointwise f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)
  elements (x, y) = [x, y]

infty :: (Fractional t) => t
infty = 1/0

instance VectorStuff ℝ3 where
  uniformV x = (x, x, x)
  corners (p1@(x1, y1, z1), p2@(x2, y2, z2)) =
    [ p1
    , (x1, y2, z1)
    , (x2, y2, z1)
    , (x2, y1, z1)
    , (x1, y1, z2)
    , (x2, y1, z2)
    , (x1, y2, z2)
    , p2
    ]
  pointwise f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)
  elements (x, y, z) = [x, y, z]

bounding :: Ord a => (a, a) -> [a] -> (a, a)
bounding e [] = e
bounding _ (a : as) = coerce $ sconcat $ fmap (Min &&& Max) $ a :| as

intersectBoxes
    :: (VectorStuff a) => (a, a) -> [(a, a)] -> (a, a)
intersectBoxes def [] = def
intersectBoxes _ (b : boxes)
  = foldr (biapp (pointwise max) (pointwise min)) b boxes


biapp :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
biapp f g (a1, b1) (a2, b2) = (f a1 a2, g b1 b2)


-- | An empty box.
emptyBox :: AdditiveGroup vec => (vec, vec)
emptyBox = (zeroV, zeroV)

-- | Define a Box3 around all of the given points.
pointsBox :: (AdditiveGroup vec, VectorStuff vec) => [vec] -> (vec, vec)
pointsBox [] = emptyBox
pointsBox as = (foldr1 (pointwise min) as, foldr1 (pointwise max) as)

-- | Is a Box3 empty?
-- | Really, this checks if it is one dimensional, which is good enough.
isEmpty :: (Eq a, AdditiveGroup a) => (a, a) -> Bool
isEmpty (v1, v2) = (v1 ^-^ v2) == zeroV

-- | Increase a boxes size by a rounding value.
outsetBox :: (AdditiveGroup a, VectorStuff a) => ℝ -> (a, a) -> (a, a)
outsetBox r (a, b) = (a ^-^ uniformV r, b ^+^ uniformV r)

-- Get a Box3 around the given object.
getBoxShared :: forall obj vec. (Eq vec, VectorStuff vec, Object obj vec, InnerSpace vec, Fractional (Scalar vec), ComponentWiseMultable vec) => SharedObj obj vec -> (vec, vec)
-- Primitives
-- (Rounded) CSG
getBoxShared (Complement _) = (uniformV (-infty), uniformV infty)
getBoxShared (UnionR r symbObjs)
  = outsetBox r
  $ pointsBox
  $ foldMap corners
  $ filter (not . isEmpty)
  $ fmap getBox symbObjs
getBoxShared (DifferenceR _ symbObj _)  = getBox symbObj
getBoxShared (IntersectR _ symbObjs) =
  intersectBoxes (uniformV (-infty), uniformV infty) $
    fmap getBox symbObjs
-- -- Simple transforms
getBoxShared (Translate v symbObj) =
    let (a :: vec, b) = getBox symbObj
     in (a ^+^ v, b ^+^ v)
getBoxShared (Scale s symbObj) =
    let
        (a :: vec, b) = getBox symbObj
        sa = s ⋯* a
        sb = s ⋯* b
     in pointsBox [sa, sb]
getBoxShared (Mirror v symbObj) =
  pointsBox $ fmap (reflect v) $ corners $ getBox symbObj
-- Boundary mods
getBoxShared (Shell w symbObj) =
    outsetBox (w/2) $ getBox symbObj
getBoxShared (Outset d symbObj) =
    outsetBox d $ getBox symbObj
-- Misc
getBoxShared (EmbedBoxedObj (_,box)) = box
