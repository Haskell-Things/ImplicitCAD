-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

{-# LANGUAGE KindSignatures, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, RankNTypes, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.StateC (getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, mapMaybeM, StateC) where

import Graphics.Implicit.ExtOpenScad.Definitions

import qualified Data.Map as Map
import           Control.Monad.State (StateT, get, put, modify, liftIO)
import           System.FilePath((</>))
import           Control.Monad.IO.Class (MonadIO)

type CompState = (VarLookup, [OVal], FilePath, (), ())
type StateC = StateT CompState IO

getVarLookup :: StateC VarLookup
getVarLookup = fmap (\(a,_,_,_,_) -> a) get

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup = modify . (\f (a,b,c,d,e) -> (f a, b, c, d, e))

lookupVar :: String -> StateC (Maybe OVal)
lookupVar name = do
    varlookup <- getVarLookup
    return $ Map.lookup name varlookup

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
    put (a,b, path </> pathShift, d, e)
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

errorC :: forall (m :: * -> *) a. (Show a, MonadIO m) => a -> [Char] -> m ()
errorC lineN err = liftIO $ putStrLn $ "At " ++ show lineN ++ ": " ++ err

mapMaybeM :: forall t (m :: * -> *) a. Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
mapMaybeM f (Just a) = do
    b <- f a
    return (Just b)
mapMaybeM _ Nothing = return Nothing
