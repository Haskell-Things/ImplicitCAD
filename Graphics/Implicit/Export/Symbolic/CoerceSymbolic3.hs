-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- We just want to export the instance...
module Graphics.Implicit.Export.Symbolic.CoerceSymbolic3 (coerceSymbolic3) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Definitions

import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import Graphics.Implicit.Export.Symbolic.CoerceSymbolic2

coerceSymbolic3 :: SymbolicObj3 -> BoxedObj3
coerceSymbolic3 (EmbedBoxedObj3 boxedObj) = boxedObj
coerceSymbolic3 (Rect3R r a b) = rect3R r a b
coerceSymbolic3 (Sphere r ) = sphere r
coerceSymbolic3 (UnionR3 r objs) = unionR r (map coerceSymbolic3 objs)
coerceSymbolic3 (IntersectR3 r objs) = intersectR r (map coerceSymbolic3 objs)
coerceSymbolic3 (DifferenceR3 r objs) = differenceR r (map coerceSymbolic3 objs)
coerceSymbolic3 (Complement3 obj) = complement $ coerceSymbolic3 obj
coerceSymbolic3 (Shell3 w obj) = shell w $ coerceSymbolic3 obj
coerceSymbolic3 (Translate3 v obj) = translate v $ coerceSymbolic3 obj
coerceSymbolic3 (Scale3 s obj) = scale s $ coerceSymbolic3 obj
coerceSymbolic3 (Outset3 d obj) = outset d $ coerceSymbolic3 obj
coerceSymbolic3 (Rotate3 rot obj) = rotate3 rot $ coerceSymbolic3 obj
coerceSymbolic3 (Rotate3V rot axis obj) = rotate3v rot axis $ coerceSymbolic3 obj
coerceSymbolic3 (ExtrudeR r obj h) = extrudeR r (coerceSymbolic2 obj) h
coerceSymbolic3 (ExtrudeRMod r mod obj h) = extrudeRMod r mod (coerceSymbolic2 obj) h
coerceSymbolic3 (ExtrudeOnEdgeOf obj1 obj2) = extrudeOnEdgeOf (coerceSymbolic2 obj1) (coerceSymbolic2 obj2)

