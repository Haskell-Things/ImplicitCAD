-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.StateC (getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, mapMaybeM, StateC) where

import Prelude(FilePath, IO, String, Maybe(Just, Nothing), Show, Monad, fmap, (.), ($), (++), return, putStrLn, show)

import Graphics.Implicit.ExtOpenScad.Definitions(VarLookup, OVal)

import Data.Map (lookup)
import Control.Monad.State (StateT, get, put, modify, liftIO)
import System.FilePath((</>))
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

-- This is the state machine. It contains the variables, their values, the path, and... ?
type CompState = (VarLookup, [OVal], FilePath, (), ())
type StateC = StateT CompState IO

getVarLookup :: StateC VarLookup
getVarLookup = fmap (\(a,_,_,_,_) -> a) get

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup = modify . (\f (a,b,c,d,e) -> (f a, b, c, d, e))

lookupVar :: String -> StateC (Maybe OVal)
lookupVar name = do
    varlookup <- getVarLookup
    return $ lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals = modify (\(a,b,c,d,e) -> (a, vals ++ b,c,d,e))

getVals :: StateC [OVal]
getVals = do
    (_,b,_,_,_) <- get
    return b

putVals :: [OVal] -> StateC ()
putVals vals = do
    (a,_,c,d,e) <- get
    put (a,vals,c,d,e)

withPathShiftedBy :: FilePath -> StateC a -> StateC a
withPathShiftedBy pathShift s = do
    (a,b,path,d,e) <- get
    put (a, b, path </> pathShift, d, e)
    x <- s
    (a',b',_,d',e') <- get
    put (a', b', path, d', e')
    return x

getPath :: StateC FilePath
getPath = do
    (_,_,c,_,_) <- get
    return c

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
    path <- getPath
    return $ path </> relPath

errorC :: forall (m :: Type -> Type) a. (Show a, MonadIO m) => a -> a -> String -> m ()
errorC lineN columnN err = liftIO $ putStrLn $ "On line " ++ show lineN ++ ", column " ++ show columnN ++ ": " ++ err
{-# INLINABLE errorC #-}

mapMaybeM :: forall t (m :: Type -> Type) a. Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
mapMaybeM f (Just a) = do
    b <- f a
    return (Just b)
mapMaybeM _ Nothing = return Nothing
