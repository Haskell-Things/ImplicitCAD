-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why are these required?
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(String, Either(Left, Right), IO, ($), fmap, return)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)

import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, ScadOpts, Message(Message), MessageType(SyntaxError), CompState(CompState))

import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)

import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)

import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)

import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import Text.Parsec.Error (errorPos, errorMessages, showErrorMessages)

import Control.Monad (mapM_)

import Control.Monad.State.Lazy (runStateT)

import System.Directory (getCurrentDirectory)

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: ScadOpts -> String -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
runOpenscad scadOpts source =
    let
        initial =  defaultObjects
        rearrange :: (t, CompState) -> (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
        rearrange (_, CompState (varlookup, ovals, _, messages, _)) = (varlookup, obj2s, obj3s, messages) where
                                  (obj2s, obj3s, _) = divideObjs ovals
        show' err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
        mesg e = Message SyntaxError (sourcePosition $ errorPos e) $ show' e
    in case parseProgram "" source of
        Left e -> return (initial, [], [], [mesg e])
        Right sts -> fmap rearrange
            $ (\sts' -> do
                path <- getCurrentDirectory
                runStateT sts' $ CompState (initial, [], path, [], scadOpts)
            )
            $ mapM_ runStatementI sts
