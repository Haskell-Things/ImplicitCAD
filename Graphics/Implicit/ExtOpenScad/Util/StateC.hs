{-# LANGUAGE ViewPatterns, RankNTypes, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.StateC where

import Graphics.Implicit.ExtOpenScad.Definitions

import qualified Data.Map as Map
import           Control.Monad.State (StateT, get, put, modify, liftIO)
import           System.FilePath((</>))


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
    (a,b,c,d,e) <- get
    return b

putVals :: [OVal] -> StateC ()
putVals vals = do
    (a,b,c,d,e) <- get
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
    (a,b,c,d,e) <- get
    return c

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
    path <- getPath
    return $ path </> relPath

errorC lineN err = liftIO $ putStrLn $ "At " ++ show lineN ++ ": " ++ err

mapMaybeM f (Just a) = do
    b <- f a
    return (Just b)
mapMaybeM _ Nothing = return Nothing
