{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ3, ℝ2, ℝ)

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class Object obj vec | obj -> vec where
    emptySpace  :: obj
    fullSpace   :: obj
    complement  :: obj -> obj
    unionR      :: ℝ -> [obj] -> obj
    differenceR :: ℝ -> obj -> [obj] -> obj
    intersectR  :: ℝ -> [obj] -> obj
    translate   :: vec -> obj -> obj
    mirror      :: vec -> obj -> obj
    scale       :: vec -> obj -> obj
    outset      :: ℝ -> obj -> obj
    shell       :: ℝ -> obj -> obj
    getBox      :: obj -> (vec, vec)
    getImplicit :: obj -> (vec -> ℝ)
    implicit    :: (vec -> ℝ) -> (vec, vec) -> obj


instance Object SymbolicObj2 ℝ2
instance Object SymbolicObj3 ℝ3

