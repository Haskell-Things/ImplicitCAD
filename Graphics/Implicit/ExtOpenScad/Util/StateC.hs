{-# LANGUAGE ViewPatterns, RankNTypes, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Util.StateC where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.ArgParser

import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad.State (State,StateT, get, put, modify, liftIO)

getVarLookup :: StateC VarLookup
getVarLookup = fmap (\(a,b) -> a) get

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup = modify . (\f (a,b) -> (f a, b))

lookupVar :: String -> StateC (Maybe OVal)
lookupVar name = do
	varlookup <- getVarLookup
	return $ Map.lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals = modify (\(a,b) -> (a, vals ++ b))

errorC lineN err = liftIO $ putStrLn $ "At " ++ show lineN ++ ": " ++ err

mapMaybeM f (Just a) = do
	b <- f a
	return (Just b)
mapMaybeM f Nothing = return Nothing
