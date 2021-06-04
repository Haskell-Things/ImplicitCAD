{- ORMOLU_DISABLE -}
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Graphics.Implicit.ObjectUtil.GetImplicitShared (getImplicitShared, normalize) where

import {-# SOURCE #-} Graphics.Implicit.Primitives (Object(getImplicit'))

import Prelude (flip, (-), (*), (>), (<), (&&), (/), product, abs, (**), fmap, (.), negate, ($), const)

import Graphics.Implicit.Definitions
    ( objectRounding, ObjectContext, SharedObj(Empty, Full, Complement, UnionR, IntersectR, DifferenceR, Translate, Scale, Mirror, Shell, Outset, EmbedBoxedObj, WithRounding), ComponentWiseMultable((⋯/)), ℝ, minℝ )

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
    => ObjectContext
    -> SharedObj obj (f ℝ)
    -> f ℝ
    -> ℝ
getImplicitShared _ Empty = const infty
getImplicitShared _ Full = const $ -infty
getImplicitShared ctx (Complement symbObj) =
  negate . getImplicit' ctx symbObj
getImplicitShared ctx (UnionR _ []) =
  getImplicitShared @obj ctx Empty
getImplicitShared ctx (UnionR r symbObjs) = \p ->
  rminimum r $ fmap (flip (getImplicit' ctx) p) symbObjs
getImplicitShared ctx (IntersectR _ []) =
  getImplicitShared @obj ctx Full
getImplicitShared ctx (IntersectR r symbObjs) = \p ->
  rmaximum r $ fmap (flip (getImplicit' ctx) p) symbObjs
getImplicitShared ctx (DifferenceR _ symbObj []) =
  getImplicit' ctx symbObj
getImplicitShared ctx (DifferenceR r symbObj symbObjs) =
    let headObj = getImplicit' ctx symbObj
    in
      \p -> do
        let
          maxTail = rmaximum r
                  $ fmap (flip (getImplicitShared ctx) p . Complement) symbObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicitShared ctx (Translate v symbObj) = \p ->
  getImplicit' ctx symbObj (p - v)
getImplicitShared ctx (Scale s symbObj) = \p ->
  normalize s * getImplicit' ctx symbObj (p ⋯/ s)
getImplicitShared ctx (Mirror v symbObj) =
    getImplicit' ctx symbObj . reflect v
-- Boundary mods
getImplicitShared ctx (Shell w symbObj) = \p ->
  abs (getImplicit' ctx symbObj p) - w/2
getImplicitShared ctx (Outset d symbObj) = \p ->
  getImplicit' ctx symbObj p - d
-- Misc
getImplicitShared _ (EmbedBoxedObj (obj,_)) = obj
getImplicitShared ctx (WithRounding r obj) = getImplicit' (ctx { objectRounding = r }) obj
