
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Export.BoxedObj3 where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingCubes
import Graphics.Implicit.Export.Util

instance DiscreteAproxable BoxedObj3 TriangleMesh where
	discreteAprox res (obj,(a,b)) = getMesh a b res obj

instance DiscreteAproxable BoxedObj3 NormedTriangleMesh where
	discreteAprox res (obj,(a,b)) = map (normTriangle res obj) $ getMesh a b res obj



