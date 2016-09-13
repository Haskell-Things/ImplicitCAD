-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.StateC (getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, mapMaybeM, StateC, CompState(CompState), languageOptions) where

import Prelude(FilePath, IO, String, Maybe(Just, Nothing), Show, Monad, fmap, (.), ($), (++), return, putStrLn, show)

import Graphics.Implicit.ExtOpenScad.Definitions(VarLookup, OVal, StateC, CompState(CompState), LanguageOpts, sourceLine, sourceColumn)

import Data.Map (lookup)
import Control.Monad.State (StateT, get, put, modify, liftIO)
import System.FilePath((</>))
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

getVarLookup :: StateC VarLookup
getVarLookup = fmap (\(CompState (a,_,_,_)) -> a) get

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup = modify . (\f (CompState (a,b,c,d)) -> CompState (f a, b, c, d))

-- | Perform a variable lookup
lookupVar :: String -> StateC (Maybe OVal)
lookupVar name = do
    varlookup <- getVarLookup
    return $ lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals = modify (\(CompState (a,b,c,d)) -> CompState (a, vals ++ b, c, d))

getVals :: StateC [OVal]
getVals = do
    (CompState (_,b,_,_)) <- get
    return b

putVals :: [OVal] -> StateC ()
putVals vals = do
    (CompState (a,_,c,d)) <- get
    put $ CompState (a,vals,c,d)

withPathShiftedBy :: FilePath -> StateC a -> StateC a
withPathShiftedBy pathShift s = do
    (CompState (a,b,path,d)) <- get
    put $ CompState (a, b, path </> pathShift, d)
    x <- s
    (CompState (a',b',_,d')) <- get
    put $ CompState (a', b', path, d')
    return x

-- | Return the path stored in the state.
getPath :: StateC FilePath
getPath = do
    (CompState (_,_,path,_)) <- get
    return path

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
    path <- getPath
    return $ path </> relPath

languageOptions :: StateC LanguageOpts
languageOptions = do
    (CompState (_, _, _, opts)) <- get
    return opts

addMesg :: Message -> StateC ()
addMesg = modify . (\message (a, b, c, d, messages) -> (a, b, c, d, messages ++ [message]))

addMessage :: MessageType -> SourcePosition -> String -> StateC ()
addMessage mtype pos text = addMesg $ Message mtype pos text

--errorC :: forall (m :: Type -> Type) a. (Show a, MonadIO m) => a -> a -> String -> m ()
errorC sourcePos err = do
    liftIO $ putStrLn $ "At " ++ show sourcePos ++ ": " ++ err
    addMessage Error sourcePos err
{-# INLINABLE errorC #-}

mapMaybeM :: forall t (m :: Type -> Type) a. Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
mapMaybeM f (Just a) = do
    b <- f a
    return (Just b)
mapMaybeM _ Nothing = return Nothing
