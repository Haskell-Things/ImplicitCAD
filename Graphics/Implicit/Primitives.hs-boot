{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Implicit.Primitives (Object(getBox, getImplicit'), getImplicit) where

import Graphics.Implicit.Definitions (ObjectContext, SymbolicObj2, SymbolicObj3, SharedObj, ℝ)
import Control.Lens (Prism')
import Prelude (Applicative, Eq, Ord, Num)
import Linear (V2, V3)

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class ( Applicative f
      , Eq a
      , Eq (f a)
      , Ord a
      , Num a
      , Num (f a))
      => Object obj f a | obj -> f a
  where
    _Shared :: Prism' obj (SharedObj obj f a)
    getBox       :: obj -> (f a, f a)
    getImplicit' :: ObjectContext -> obj -> (f a -> a)
    implicit     :: (f a -> a) -> (f a, f a) -> obj

getImplicit :: Object obj f a => obj -> (f a -> a)

instance Object SymbolicObj2 V2 ℝ
instance Object SymbolicObj3 V3 ℝ

