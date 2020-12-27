{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions (ObjectContext, SymbolicObj2, SymbolicObj3, SharedObj, ℝ3, ℝ2, ℝ)
import Control.Lens (Prism')
import Prelude (Num)

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class Num vec => Object obj vec | obj -> vec where
    _Shared :: Prism' obj (SharedObj obj vec)
    getBox       :: obj -> (vec, vec)
    getImplicit' :: ObjectContext -> obj -> (vec -> ℝ)

getImplicit :: Object obj vec => obj -> (vec -> ℝ)

instance Object SymbolicObj2 ℝ2
instance Object SymbolicObj3 ℝ3

