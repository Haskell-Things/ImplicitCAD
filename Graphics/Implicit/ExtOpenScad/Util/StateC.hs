-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- For the signature of mapMaybeM.
{-# LANGUAGE KindSignatures #-}

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}


module Graphics.Implicit.ExtOpenScad.Util.StateC (addMessage, getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, warnC, mapMaybeM, scadOptions) where

import Prelude(FilePath, String, Maybe(Just, Nothing), Monad, (.), ($), (++), return)

import Graphics.Implicit.ExtOpenScad.Definitions(VarLookup(VarLookup), OVal, Symbol, SourcePosition, Message(Message), MessageType(Error, Warning), ScadOpts, StateC, CompState(CompState))

import Data.Map (lookup)

import "monads-tf" Control.Monad.State (get, put, modify)

import System.FilePath((</>))

import Data.Kind (Type)

getVarLookup :: StateC VarLookup
getVarLookup = do
  (CompState (varlookup,_,_,_,_)) <- get
  return varlookup

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup = modify . (\f (CompState (a,b,c,d,e)) -> CompState (f a, b, c, d, e))

-- | Perform a variable lookup
--   FIXME: generate a warning when we look up a variable that is not present.
lookupVar :: Symbol -> StateC (Maybe OVal)
lookupVar name = do
    (VarLookup varlookup) <- getVarLookup
    return $ lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals= modify (\(CompState (a,b,c,d,e)) -> CompState (a, vals ++ b, c, d, e))

getVals :: StateC [OVal]
getVals = do
    (CompState (_,vals,_,_,_)) <- get
    return vals

putVals :: [OVal] -> StateC ()
putVals vals = do
    (CompState (a,_,c,d,e)) <- get
    put $ CompState (a,vals,c,d,e)

withPathShiftedBy :: FilePath -> StateC a -> StateC a
withPathShiftedBy pathShift s = do
    (CompState (a,b,path,d,e)) <- get
    put $ CompState (a, b, path </> pathShift, d, e)
    x <- s
    (CompState (a',b',_,d',e')) <- get
    put $ CompState (a', b', path, d', e')
    return x

-- | Return the path stored in the state.
getPath :: StateC FilePath
getPath = do
    (CompState (_,_,path,_,_)) <- get
    return path

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
    path <- getPath
    return $ path </> relPath

scadOptions :: StateC ScadOpts
scadOptions = do
  (CompState (_, _, _, _, opts)) <- get
  return opts

addMesg :: Message -> StateC ()
addMesg = modify . (\message (CompState (a, b, c, messages, d)) -> CompState (a, b, c, messages ++ [message], d))

addMessage :: MessageType -> SourcePosition -> String -> StateC ()
addMessage mtype pos text = addMesg $ Message mtype pos text

errorC :: SourcePosition -> String -> StateC ()
errorC = addMessage Error
{-# INLINABLE errorC #-}

warnC :: SourcePosition -> String -> StateC ()
warnC = addMessage Warning
{-# INLINABLE warnC #-}

mapMaybeM :: forall t (m :: Type -> Type) a. Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
mapMaybeM f (Just a) = do
    b <- f a
    return (Just b)
mapMaybeM _ Nothing = return Nothing
