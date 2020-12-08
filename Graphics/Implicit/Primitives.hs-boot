{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions (ComponentWiseMultable, SymbolicObj2, SymbolicObj3, SharedObj, ℝ3, ℝ2, ℝ)
import Control.Lens (Prism')
import Data.AdditiveGroup ( AdditiveGroup )

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class (ComponentWiseMultable vec, AdditiveGroup vec) => Object obj vec | obj -> vec where
    _Shared :: Prism' obj (SharedObj obj vec)
    getBox      :: obj -> (vec, vec)
    getImplicit :: obj -> (vec -> ℝ)


instance Object SymbolicObj2 ℝ2
instance Object SymbolicObj3 ℝ3

