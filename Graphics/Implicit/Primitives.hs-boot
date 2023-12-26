{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- due to GHC 8.7.10 (and lesser) warning about Space
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Graphics.Implicit.Primitives (Object(getBox, getImplicit', Space, _Shared), getImplicit, emptySpace, fullSpace,outset) where

import Graphics.Implicit.Definitions (ObjectContext, SymbolicObj2, SymbolicObj3, SharedObj, ℝ)
import Control.Lens (Prism')
import Data.Kind (Type)
import Prelude (Applicative, Eq, Foldable, Num)
import Linear (V2, V3)

-- See the non-source version of "Graphics.Implicit.Primitives" for
-- documentation of this class.
class ( Applicative f
      , Eq a
      , Eq (f a)
      , Foldable f
      , Num a
      , Num (f a)
      )
      => Object obj f a | obj -> f a
  where
    type Space obj :: Type -> Type
    _Shared :: Prism' obj (SharedObj obj f a)
    getBox       :: obj -> (f a, f a)
    getImplicit' :: ObjectContext -> obj -> (f a -> a)
    canonicalize :: obj -> obj

getImplicit :: Object obj f a => obj -> (f a -> a)

instance Object SymbolicObj2 V2 ℝ
instance Object SymbolicObj3 V3 ℝ

emptySpace, fullSpace :: Object obj f a => obj
outset :: Object obj f a => ℝ -> obj -> obj
