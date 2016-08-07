-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why are these required?
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(String, Either(Left, Right), IO, ($), fmap)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, LanguageOpts, alternateParser)
import qualified Graphics.Implicit.ExtOpenScad.Parser.Statement as Orig (parseProgram)
import qualified Graphics.Implicit.ExtOpenScad.Parser.AltStatement as Alt (parseProgram)
import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)
import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Util.StateC (CompState(CompState))
import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import Text.Parsec.Error (ParseError)
import Control.Monad (mapM_)
import Control.Monad.State (runStateT)
import System.Directory (getCurrentDirectory)

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: LanguageOpts -> String -> Either ParseError (IO (VarLookup, [SymbolicObj2], [SymbolicObj3]))
runOpenscad languageOpts source =
    let
        
        initial =  defaultObjects
        rearrange :: forall t. (t, CompState) -> (VarLookup, [SymbolicObj2], [SymbolicObj3])
        rearrange (_, (CompState (varlookup, ovals, _, _))) = (varlookup, obj2s, obj3s) where
                                  (obj2s, obj3s, _) = divideObjs ovals
        parseProgram = if alternateParser languageOpts then Alt.parseProgram else Orig.parseProgram
    in case parseProgram "" source of
        Left e -> Left e
        Right sts -> Right
            $ fmap rearrange
            $ (\sts' -> do
                path <- getCurrentDirectory
                runStateT sts' $ CompState (initial, [], path, languageOpts)
            )
            $ mapM_ runStatementI sts
