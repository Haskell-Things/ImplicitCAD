-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why are these required?
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(Char, Either(Left, Right), IO, ($), fmap)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)
import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import qualified Text.Parsec.Error as Parsec (ParseError)
import qualified Control.Monad as Monad (mapM_)
import qualified Control.Monad.State as State (runStateT)
import qualified System.Directory as Dir (getCurrentDirectory)

-- Small wrapper to handle parse errors, etc.
runOpenscad :: [Char] -> Either Parsec.ParseError (IO (VarLookup, [SymbolicObj2], [SymbolicObj3]))
runOpenscad s =
    let
        initial =  defaultObjects
        rearrange :: forall t t1 t2 t3 t4. (t, (t4, [OVal], t1, t2, t3)) -> (t4, [SymbolicObj2], [SymbolicObj3])
        rearrange (_, (varlookup, ovals, _ , _ , _)) = (varlookup, obj2s, obj3s) where
                                  (obj2s, obj3s, _ ) = divideObjs ovals
    in case parseProgram "" s of
        Left e -> Left e
        Right sts -> Right
            $ fmap rearrange
            $ (\sts' -> do
                path <- Dir.getCurrentDirectory
                State.runStateT sts' (initial, [], path, (), () )
            )
            $ Monad.mapM_ runStatementI sts
