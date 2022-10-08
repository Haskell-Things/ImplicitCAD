{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Util.StateC (addMessage, getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, warnC, scadOptions) where

import Prelude(FilePath, Maybe, ($), (<>), pure)

import Graphics.Implicit.ExtOpenScad.Definitions(VarLookup(VarLookup), OVal, Symbol, SourcePosition, Message(Message), MessageType(Error, Warning), ScadOpts, StateC, CompState(scadVars, oVals, sourceDir))

import Data.Map (lookup)

import Data.Text.Lazy (Text)

import Control.Monad.State (modify, gets)

import System.FilePath((</>))
import Control.Monad.Writer (tell)
import Control.Monad.Reader.Class (ask)

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

-- Add a single message to the list of messages being returned
addMesg :: Message -> StateC ()
addMesg m = tell [m]

addMessage :: MessageType -> SourcePosition -> Text -> StateC ()
addMessage mtype pos text = addMesg $ Message mtype pos text

errorC :: SourcePosition -> Text -> StateC ()
errorC = addMessage Error
{-# INLINABLE errorC #-}

warnC :: SourcePosition -> Text -> StateC ()
warnC = addMessage Warning
{-# INLINABLE warnC #-}

-- Get the ScadOpts from the Reader in ImplicitCadM
scadOptions :: StateC ScadOpts
scadOptions = ask