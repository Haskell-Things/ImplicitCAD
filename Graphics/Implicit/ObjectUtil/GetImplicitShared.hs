{-# LANGUAGE TypeApplications #-}
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.ObjectUtil.GetImplicitShared where

import {-# SOURCE #-} Graphics.Implicit.Primitives
    ( Object(getImplicit) )
import Prelude
import Graphics.Implicit.Definitions
    ( SharedObj(..), ComponentWiseMultable((⋯/)), ℝ, minℝ )
import Graphics.Implicit.MathUtil
    ( rmax, rmaximum, rminimum, reflect )
-- Use getImplicit2 for handling extrusion of 2D shapes to 3D.
import Data.VectorSpace ((<.>), magnitude, Scalar, InnerSpace, AdditiveGroup((^-^)))
import Graphics.Implicit.ObjectUtil.GetBoxShared


normalize :: forall vec. (VectorStuff vec, InnerSpace vec, Scalar vec ~ ℝ) => vec -> Scalar vec
normalize v =
  let all1s = uniformV @vec 1
   in abs (product (elements v)) ** (1 / (all1s <.> all1s))


-- Get a function that describes the surface of the object.
getImplicitShared :: (VectorStuff vec, Object obj vec, InnerSpace vec, Fractional (Scalar vec), Scalar vec ~ ℝ, ComponentWiseMultable vec) => SharedObj obj vec -> vec -> ℝ
getImplicitShared (Complement symbObj) =
  negate . getImplicit symbObj

getImplicitShared (UnionR r symbObjs) =
  \p -> rminimum r $ fmap ($p) $ getImplicit <$> symbObjs

getImplicitShared (IntersectR r symbObjs) =
  \p -> rmaximum r $ fmap ($p) $ getImplicit <$> symbObjs
getImplicitShared (DifferenceR _ symbObj []) =
  getImplicit symbObj
getImplicitShared (DifferenceR r symbObj symbObjs) =
    let
        tailObjs = getImplicit <$> symbObjs
        headObj = getImplicit symbObj
        complement obj' p = - obj' p
    in
      \p -> do
        let
          maxTail = rmaximum r $ fmap ($p) $ complement <$> tailObjs
        if maxTail > -minℝ && maxTail < minℝ
          then rmax r (headObj p) minℝ
          else rmax r (headObj p) maxTail

-- Simple transforms
getImplicitShared (Translate v symbObj) =
    let
        obj = getImplicit symbObj
    in
        \p -> obj (p ^-^ v)
getImplicitShared (Scale s symbObj) =
    let
        obj = getImplicit symbObj
        k = normalize s
    in
        \p -> k * obj (p ⋯/ s)
getImplicitShared (Mirror v symbObj) =
    getImplicit symbObj . reflect v
-- Boundary mods
getImplicitShared (Shell w symbObj) =
    let
        obj = getImplicit symbObj
    in
        \p -> abs (obj p) - w/2
getImplicitShared (Outset d symbObj) =
    let
        obj = getImplicit symbObj
    in
        \p -> obj p - d
-- Misc
getImplicitShared (EmbedBoxedObj (obj,_)) = obj
