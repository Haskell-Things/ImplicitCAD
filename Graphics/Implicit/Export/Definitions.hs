-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Export.Definitions where

import Graphics.Implicit.Definitions

-- | There is a discrete way to aproximate this object.
--   eg. Aproximating a 3D object with a tirangle mesh
--       would be DiscreteApproxable Obj3 TriangleMesh
class DiscreteAproxable obj aprox where
    discreteAprox :: ℝ -> obj -> aprox
