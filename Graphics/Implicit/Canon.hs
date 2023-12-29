{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, 2017, 2018, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- | This module implements canonicalization pass that
-- * eliminates identities
-- * merges consecutive transformations like transform . transform into one
-- * prevents invalid transformations like scaling by zero that would
--   otherwise result in NaNs down the pipe
-- * turns degenerate objects into empty space (i.e. circle 0, cube (pure 0))

{-# LANGUAGE Rank2Types #-}
-- pattern Shared
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.Implicit.Canon
  ( canonicalize2
  , canonicalize3
  , fmapObj2
  , fmapObj3
  , fmapSharedObj
  , rewriteUntilIrreducible
  , EqObj((=^=))
  ) where

import Linear
  ( V2(V2)
  , V3(V3)
  , V4(V4)
  )

import Prelude
  ( Bool
      ( False
      , True
      )
  , Either(Left)
  , Eq((==))
  , Maybe(Just)
  , Num
      ( (*)
      , (+)
      )
  , Ord((<))
  , length
  , ($)
  , (&&)
  , (<$>)
  )

import Graphics.Implicit.Definitions
  ( ExtrudeMScale
      ( C1
      , C2
      , Fn
      )
  , SharedObj
      ( Complement
      , DifferenceR
      , EmbedBoxedObj
      , Empty
      , Full
      , IntersectR
      , Mirror
      , Outset
      , Scale
      , Shell
      , Translate
      , UnionR
      , WithRounding
      )
  , SymbolicObj2
      ( Circle
      , Polygon
      , Rotate2
      , Shared2
      , Square
      , Transform2
      , Slice
      )
  , SymbolicObj3
      ( Cube
      , Cylinder
      , Extrude
      , ExtrudeM
      , ExtrudeOnEdgeOf
      , Rotate3
      , RotateExtrude
      , Shared3
      , Sphere
      , Transform3
      , Torus
      , Ellipsoid
      , BoxFrame
      , Transform3
      )
  , hasZeroComponent
  )
import {-# SOURCE #-} Graphics.Implicit.Primitives
  ( Object(_Shared)
  , emptySpace
  , fullSpace
  )

import Control.Lens
  ( preview
  , (#)
  )

-- | A pattern that abstracts over 'Shared2' and 'Shared3'.
-- Can't be in hs-boot https://gitlab.haskell.org/ghc/ghc/-/issues/14478
-- so we duplicate it here
pattern Shared :: (Object obj f a) => SharedObj obj f a -> obj
pattern Shared v <- (preview _Shared -> Just v)
  where
    Shared v = _Shared # v

-- | Map over @SharedObj@ and its underlying objects
--
-- This resembles bimap from Bifunctor but the structure
-- of SharedObj doesn't allow us to define Bifunctor instance
-- as we need to map over the first type argument (obj) and not f and a.
fmapSharedObj
  :: forall obj f a
   . (Object obj f a)
  => (obj -> obj)
  -> (obj -> obj)
  -> obj
  -> obj
{-# INLINABLE fmapSharedObj #-}
fmapSharedObj _ g (Shared Empty) = g emptySpace
fmapSharedObj _ g (Shared Full) = g fullSpace
fmapSharedObj f g (Shared (Complement o)) = g $ Shared $ Complement (f o)
fmapSharedObj f g (Shared (UnionR r os)) = g $ Shared $ UnionR r $ f <$> os
fmapSharedObj f g (Shared (DifferenceR r o os)) = g $ Shared $ DifferenceR r (f o) $ f <$> os
fmapSharedObj f g (Shared (IntersectR r os)) = g $ Shared $ IntersectR r $ f <$> os
fmapSharedObj f g (Shared (Translate by o)) = g $ Shared $ Translate by (f o)
fmapSharedObj f g (Shared (Scale by o)) = g $ Shared $ Scale by (f o)
fmapSharedObj f g (Shared (Mirror by o)) = g $ Shared $ Mirror by (f o)
fmapSharedObj f g (Shared (Outset by o)) = g $ Shared $ Outset by (f o)
fmapSharedObj f g (Shared (Shell by o)) = g $ Shared $ Shell by (f o)
fmapSharedObj _ g (Shared (EmbedBoxedObj fun)) = g $ Shared $ EmbedBoxedObj fun
fmapSharedObj f g (Shared (WithRounding r o)) = g $ Shared $ WithRounding r (f o)
fmapSharedObj f _ o = f o

-- | Map over @SymbolicObj2@ and its underlying shared objects
--
-- This function is co-recursive with @fmapSharedObj@ to achieve
-- deep mapping over objects nested in @Shared2@ constructor
fmapObj2
  :: (SymbolicObj2 -> SymbolicObj2) -- ^ SymbolicObj2 transformation
  -> (SymbolicObj3 -> SymbolicObj3) -- ^ SymbolicObj3 transformation
  -> (forall obj f a . (Object obj f a) => obj -> obj) -- ^ Shared2|3 transformation
  -> SymbolicObj2
  -> SymbolicObj2
fmapObj2 f _ _ (Square v)       = f $ Square v
fmapObj2 f _ _ (Circle r)       = f $ Circle r
fmapObj2 f _ _ (Polygon ps)     = f $ Polygon ps
fmapObj2 f g s (Rotate2 r o)    = f $ Rotate2 r (fmapObj2 f g s o)
fmapObj2 f g s (Transform2 m o) = f $ Transform2 m (fmapObj2 f g s o)
fmapObj2 f g s (Slice o)        = f $ Slice (fmapObj3 g f s o)
fmapObj2 f g s (Shared2 o)      = fmapSharedObj (fmapObj2 f g s) s (Shared2 o)

-- | Map over @SymbolicObj3@ and its underlying shared objects
--
-- This function is co-recursive with @fmapSharedObj@ to achieve
-- deep mapping over objects nested in @Shared3@ constructor
fmapObj3
  :: (SymbolicObj3 -> SymbolicObj3) -- ^ SymbolicObj3 transformation
  -> (SymbolicObj2 -> SymbolicObj2) -- ^ SymbolicObj2 transformation
  -> (forall obj f a . (Object obj f a) => obj -> obj) -- ^ Shared2|3 transformation
  -> SymbolicObj3
  -> SymbolicObj3
fmapObj3 f _ _ (Cube v) = f $ Cube v
fmapObj3 f _ _ (Sphere r) = f $ Sphere r
fmapObj3 f _ _ (Cylinder r1 r2 h) = f $ Cylinder r1 r2 h
fmapObj3 f _ _ (Torus r1 r2) = f $ Torus r1 r2
fmapObj3 f _ _ (Ellipsoid a b c) = f $ Ellipsoid a b c
fmapObj3 f _ _ (BoxFrame b e) = f $ BoxFrame b e
fmapObj3 f g s (Rotate3 q o) = f $ Rotate3 q (fmapObj3 f g s o)
fmapObj3 f g s (Transform3 m o) = f $ Transform3 m (fmapObj3 f g s o)
fmapObj3 f g s (Extrude h o2) = f $ Extrude h (fmapObj2 g f s o2)
fmapObj3 f g s (ExtrudeM twist sc tr o2 h) = f (ExtrudeM twist sc tr (fmapObj2 g f s o2) h)
fmapObj3 f g s (RotateExtrude angle tr rot o2) = f (RotateExtrude angle tr rot (fmapObj2 g f s o2))
fmapObj3 f g s (ExtrudeOnEdgeOf o2a o2b) = f (ExtrudeOnEdgeOf (fmapObj2 g f s o2a) (fmapObj2 g f s o2b))
fmapObj3 f g s (Shared3 o) = fmapSharedObj (fmapObj3 f g s) s (Shared3 o)

-- | We have to define our own variant of Eq
-- which compares objects when possible
-- and returns True when we cannot compare
-- things like functions
class EqObj a where
  (=^=) :: a -> a -> Bool

instance EqObj a => EqObj [a] where
  []     =^= []     = True
  (x:xs) =^= (y:ys) = x =^= y && xs =^= ys
  _xs    =^= _ys    = False

instance (EqObj obj , Eq (f a)) => EqObj (SharedObj obj f a) where
  Empty =^= Empty = True
  Full =^= Full = True
  Complement a =^= Complement b = a =^= b
  UnionR r1 a =^= UnionR r2 b = r1 == r2 && a =^= b
  DifferenceR r1 a x =^= DifferenceR r2 b y = r1 == r2 && a =^= b && x =^= y
  IntersectR r1 a =^= IntersectR r2 b = r1 == r2 && a =^= b
  Translate x a =^= Translate y b = x == y && a =^= b
  Scale x a =^= Scale y b = x == y && a =^= b
  Mirror x a =^= Mirror y b = x == y && a =^= b
  Outset x a =^= Outset y b = x == y && a =^= b
  Shell x a =^= Shell y b = x == y && a =^= b
  EmbedBoxedObj (_fA, a) =^= EmbedBoxedObj (_fB, b) = a == b
  WithRounding x a =^= WithRounding y b = x == y && a =^= b
  _ =^= _ = False

instance EqObj ExtrudeMScale where
  C1 x =^= C1 y = x == y
  C2 x =^= C2 y = x == y
  Fn _ =^= Fn _ = True
  _ =^= _ = False

instance EqObj SymbolicObj2 where
  Square a =^= Square b = a == b
  Circle a =^= Circle b = a == b
  Polygon a =^= Polygon b = a == b
  Rotate2 x a =^= Rotate2 y b = x == y && a =^= b
  Transform2 x a =^= Transform2 y b = x == y && a =^= b
  Slice a =^= Slice b = a =^= b
  Shared2 a =^= Shared2 b = a =^= b
  _ =^= _ = False

instance EqObj SymbolicObj3 where
  Cube a =^= Cube b = a == b
  Sphere a =^= Sphere b = a == b
  Torus a1 a2 =^= Torus b1 b2 = a1 == b1 && a2 == b2
  Ellipsoid a1 b1 c1 =^= Ellipsoid a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
  Cylinder r1a r2a ha =^= Cylinder r1b r2b hb = r1a == r1b && r2a == r2b && ha == hb
  BoxFrame b1 e1 =^= BoxFrame b2 e2 = b1 == b2 && e1 == e2
  Rotate3 x a =^= Rotate3 y b = x == y && a =^= b
  Transform3 x a =^= Transform3 y b = x == y && a =^= b
  Extrude x a =^= Extrude y b = x == y && a =^= b

  ExtrudeM (Left twa) ma (Left ta) a (Left ha)
    =^=
    ExtrudeM (Left twb) mb (Left tb) b (Left hb)
      = twa == twb && ma =^= mb && ta == tb && ha == hb && a =^= b
  ExtrudeM {} =^= ExtrudeM {} = True

  RotateExtrude ra (Left ta) (Left rota) a
    =^=
    RotateExtrude rb (Left tb) (Left rotb) b
      = ra == rb && ta == tb && rota == rotb && a =^= b
  RotateExtrude {} =^= RotateExtrude {} = True

  ExtrudeOnEdgeOf a x =^= ExtrudeOnEdgeOf b y = a =^= b && x =^= y
  Shared3 a =^= Shared3 b = a =^= b
  _ =^= _ = False

-- | Rewrite the object tree until it cannot be reduced further
rewriteUntilIrreducible
  :: ( Object obj f a
     , EqObj obj)
  => (obj -> obj) -- ^ SymbolicObjN transformation
  -> obj
  -> obj
rewriteUntilIrreducible fRew ast =
  let
    step = fRew ast
  in
    if step =^= ast
    then step
    else rewriteUntilIrreducible fRew step

-- | Canonicalize @SymbolicObj2@ tree
canonicalize2 :: SymbolicObj2 -> SymbolicObj2
canonicalize2 = rewriteUntilIrreducible $ fmapObj2 canon2 canon3 canonShared

-- | Canonicalize @SymbolicObj3@ tree
canonicalize3 :: SymbolicObj3 -> SymbolicObj3
canonicalize3 = rewriteUntilIrreducible $ fmapObj3 canon3 canon2 canonShared

{-# ANN canon2 "HLint: ignore Use record patterns" #-}
{-# ANN canon3 "HLint: ignore Use record patterns" #-}

-- | Rewrite rules for @SymbolicObj2@
canon2 :: SymbolicObj2 -> SymbolicObj2
canon2 (Square v) | hasZeroComponent v = emptySpace
canon2 (Circle 0) = emptySpace
canon2 (Polygon ps) | length ps < 3 = emptySpace
canon2 (Rotate2 0 o) = o
-- TOOD(srk): this "fixes" (more like hides) the problem
-- with polygon under rotation described in #449
-- so we keep it disabled for now
-- needs import Data.Fixed (mod') and Prelude (pi)
-- canon2 (Rotate2 θ o) | θ `mod'` (2*pi) == 0 = o

-- ignore if zeroes, TODO(srk): produce warning
-- TODO(srk): produce warning and ignore if we get a non-invertible matrix
canon2 (Transform2
         (V3 (V3 x _ _)
             (V3 _ y _)
             (V3 _ _ _)
         )
         o) | hasZeroComponent (V2 x y) = o
canon2 x = x

-- | Rewrite rules for @SymbolicObj3@
canon3 :: SymbolicObj3 -> SymbolicObj3
canon3 (Cube v) | hasZeroComponent v = emptySpace
canon3 (Sphere 0) = emptySpace
canon3 (Cylinder 0 _ _) = emptySpace
canon3 (BoxFrame _ 0) = emptySpace
canon3 (Extrude 0 _o2) = emptySpace
canon3 (Torus _ 0) = emptySpace
canon3 (Ellipsoid 0 _ _) = emptySpace
canon3 (Ellipsoid _ 0 _) = emptySpace
canon3 (Ellipsoid _ _ 0) = emptySpace
canon3 (Rotate3 0 o) = o
canon3 (RotateExtrude 0 _t _r _o) = emptySpace
canon3 (RotateExtrude _theta _t _r (Shared Empty)) = emptySpace
-- ignore if zeroes, TODO(srk): produce warning
-- TODO(srk): produce warning and ignore if we get a non-invertible matrix
canon3 (Transform3
         (V4 (V4 x _ _ _)
             (V4 _ y _ _)
             (V4 _ _ z _)
             (V4 _ _ _ _)
         )
         o) | hasZeroComponent (V3 x y z) = o
canon3 x = x

-- | Rewrite rules for @SharedObj@
canonShared
  :: forall obj f a
   . (Object obj f a)
  => obj
  -> obj
canonShared (Shared (Scale 1 o)) = o
canonShared (Shared (Scale v1 (Shared (Scale v2 o)))) = Shared $ Scale (v1 * v2) o
canonShared (Shared (Scale _ s@(Shared Empty))) = s
canonShared (Shared (Scale _ s@(Shared Full))) = s
-- ignore if zeroes, TODO(srk): produce warning
canonShared (Shared (Scale s o)) | hasZeroComponent s = o
canonShared (Shared (Translate 0 o)) = o
canonShared (Shared (Translate _ s@(Shared Empty))) = s
canonShared (Shared (Translate _ s@(Shared Full))) = s
canonShared (Shared (Translate v1 (Shared (Translate v2 o)))) = Shared $ Translate (v1 + v2) o

canonShared (Shared (Mirror _ (Shared Empty))) = emptySpace
canonShared (Shared (Mirror _ (Shared Full))) = fullSpace
canonShared (Shared (Outset 0 s)) = s
canonShared (Shared (Outset 0 (Shared Empty))) = emptySpace
canonShared (Shared (Outset 0 (Shared Full))) = fullSpace
canonShared (Shared (Outset v1 (Shared (Outset v2 o)))) = Shared $ Outset (v1 + v2) o
canonShared (Shared (Shell _ (Shared Full))) = fullSpace
canonShared (Shared (Shell _ (Shared Empty))) = emptySpace
canonShared (Shared (Shell _ (Shared Full))) = fullSpace
canonShared (Shared (UnionR _ [])) = emptySpace
canonShared (Shared (UnionR _ [s])) = s
canonShared (Shared (DifferenceR _ s [])) = s
canonShared (Shared (DifferenceR _ (Shared Empty) _)) = emptySpace
canonShared (Shared (IntersectR _ [])) = emptySpace
canonShared (Shared (IntersectR _ [s])) = s
canonShared x = x
