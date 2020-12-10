-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared, normalize) where

import {-# SOURCE #-} Graphics.Implicit.Primitives (Object(getImplicit))

import Prelude (flip, (-), (*), (>), (<), (&&), (/), product, abs, (**), fmap, (.), negate, ($), const)

import Graphics.Implicit.Definitions
    ( SharedObj(Empty, Full, Complement, UnionR, IntersectR, DifferenceR, Translate, Scale, Mirror, Shell, Outset, EmbedBoxedObj), ComponentWiseMultable((⋯/)), ℝ, minℝ )

import Graphics.Implicit.MathUtil (infty, rmax, rmaximum, rminimum, reflect)

-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import Graphics.Implicit.ObjectUtil.GetBoxShared (VectorStuff(elements, uniformV))

import Linear (Metric(dot))


------------------------------------------------------------------------------
-- | Normalize a dimensionality-polymorphic vector.
normalize
    :: forall f
     . (VectorStuff (f ℝ), Metric f)
    => f ℝ
    -> ℝ
normalize v =
  let all1s = uniformV @(f ℝ) 1
   in abs (product (elements v)) ** (1 / (all1s `dot` all1s))


-- Get a function that describes the surface of the object.
getImplicitShared
    :: forall obj f
     . ( Object obj (f ℝ)
       , VectorStuff (f ℝ)
       , ComponentWiseMultable (f ℝ)
       , Metric f
       )
    => SharedObj obj (f ℝ)
    -> f ℝ
    -> ℝ
getImplicitShared Empty = const infty
getImplicitShared Full = const $ -infty
getImplicitShared (Complement symbObj) =
  negate . getImplicit symbObj
getImplicitShared (UnionR _ []) =
  getImplicitShared @obj Empty
getImplicitShared (UnionR r symbObjs) = \p ->
  rminimum r $ fmap (`getImplicit` p) symbObjs
getImplicitShared (IntersectR _ []) =
  getImplicitShared @obj Full
getImplicitShared (IntersectR r symbObjs) = \p ->
  rmaximum r $ fmap (`getImplicit` p) symbObjs
getImplicitShared (DifferenceR _ symbObj []) =
  getImplicit symbObj
getImplicitShared (DifferenceR r symbObj symbObjs) =
    let headObj = getImplicit symbObj
    in
      \p -> do
        let
          maxTail = rmaximum r
                  $ fmap (flip getImplicitShared p . Complement) symbObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicitShared (Translate v symbObj) = \p ->
  getImplicit symbObj (p - v)
getImplicitShared (Scale s symbObj) = \p ->
  normalize s * getImplicit symbObj (p ⋯/ s)
getImplicitShared (Mirror v symbObj) =
    getImplicit symbObj . reflect v
-- Boundary mods
getImplicitShared (Shell w symbObj) = \p ->
  abs (getImplicit symbObj p) - w/2
getImplicitShared (Outset d symbObj) = \p ->
  getImplicit symbObj p - d
-- Misc
getImplicitShared (EmbedBoxedObj (obj,_)) = obj

