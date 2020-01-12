-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}


module Graphics.Implicit.ExtOpenScad.Util.StateC (addMessage, getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, warnC, scadOptions) where

import Prelude(FilePath, String, Maybe, ($), (<>), pure)

import Graphics.Implicit.ExtOpenScad.Definitions(VarLookup(VarLookup), OVal, Symbol, SourcePosition, Message(Message), MessageType(Error, Warning), ScadOpts, StateC, CompState(scadVars, oVals, sourceDir, messages, scadOpts))

import Data.Map (lookup)

import "monads-tf" Control.Monad.State (modify, gets)

import System.FilePath((</>))

getVarLookup :: StateC VarLookup
getVarLookup = gets scadVars

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup f = modify $ \c -> c { scadVars = f $ scadVars c }

-- | Perform a variable lookup
--   FIXME: generate a warning when we look up a variable that is not present.
lookupVar :: Symbol -> StateC (Maybe OVal)
lookupVar name = do
    (VarLookup varlookup) <- getVarLookup
    pure $ lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals = modify $ \c -> c { oVals = vals <> oVals c }

getVals :: StateC [OVal]
getVals = gets oVals

putVals :: [OVal] -> StateC ()
putVals vals = modify $ \c -> c { oVals = vals }

withPathShiftedBy :: FilePath -> StateC a -> StateC a
withPathShiftedBy pathShift s = do
  path <- getPath
  modify $ \c -> c { sourceDir = path </> pathShift }
  x <- s
  modify $ \c -> c { sourceDir = path }
  pure x

-- | Pure the path stored in the state.
getPath :: StateC FilePath
getPath = gets sourceDir

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
    path <- getPath
    pure $ path </> relPath

addMesg :: Message -> StateC ()
addMesg m = modify $ \c -> c { messages = messages c <> pure m }

addMessage :: MessageType -> SourcePosition -> String -> StateC ()
addMessage mtype pos text = addMesg $ Message mtype pos text

errorC :: SourcePosition -> String -> StateC ()
errorC = addMessage Error
{-# INLINABLE errorC #-}

warnC :: SourcePosition -> String -> StateC ()
warnC = addMessage Warning
{-# INLINABLE warnC #-}

scadOptions :: StateC ScadOpts
scadOptions = gets scadOpts

