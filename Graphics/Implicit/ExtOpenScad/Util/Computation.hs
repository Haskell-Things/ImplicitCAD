-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

-- | Utilities for dealing with computations, in particular ComputationStateModifier

module Graphics.Implicit.ExtOpenScad.Util.Computation where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions

-- | Run a list of computations!
--   We start with a state and run it through a bunch of ComputationStateModifier s.
runComputations :: ComputationState -> [ComputationStateModifier]  -> ComputationState
runComputations = foldl (\a b -> b $ a)

addObj2 :: (Monad m) => Obj2Type -> m ComputationStateModifier
addObj2 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj2s ++ [obj], obj3s)

addObj3 :: (Monad m) => Obj3Type -> m ComputationStateModifier
addObj3 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj2s, obj3s ++ [obj])

changeObjs :: (Monad m) => ([Obj2Type] -> [Obj2Type]) -> ([Obj3Type] -> [Obj3Type]) -> m ComputationStateModifier
changeObjs mod2s mod3s = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, mod2s obj2s, mod3s obj3s)

runIO ::  (Monad m) => IO() -> m ComputationStateModifier
runIO newio = return $  \ ioWrappedState -> do
		state <- ioWrappedState
		newio
		return state

noChange :: (Monad m) => m ComputationStateModifier
noChange = return id

getAndModUpObj2s :: (Monad m) => [ComputationStateModifier] 
	-> (Obj2Type -> Obj3Type)
	-> m ComputationStateModifier
getAndModUpObj2s suite obj2mod = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s, 
			 obj3s ++ (case obj2s2 of [] -> []; x:xs -> [obj2mod x])  )

getAndCompressSuiteObjs :: (Monad m) => [ComputationStateModifier] 
	-> ([Obj2Type] -> Obj2Type)
	-> ([Obj3Type] -> Obj3Type)
	-> m ComputationStateModifier
getAndCompressSuiteObjs suite obj2modifier obj3modifier = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s ++ (case obj2s2 of [] -> []; _ -> [obj2modifier obj2s2]), 
			 obj3s ++ (case obj3s2 of [] -> []; _ -> [obj3modifier obj3s2])  )

getAndTransformSuiteObjs :: (Monad m) => [ComputationStateModifier] 
	-> (Obj2Type -> Obj2Type)
	-> (Obj3Type -> Obj3Type)
	-> m ComputationStateModifier
getAndTransformSuiteObjs suite obj2modifier obj3modifier = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s ++ (map obj2modifier obj2s2),
			 obj3s ++ (map obj3modifier obj3s2)   )

