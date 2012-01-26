
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Export.BoxedObj3 where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingCubes

instance DiscreteAproxable BoxedObj3 TriangleMesh where
	discreteAprox res (obj,(a,b)) = getMesh a b res obj
