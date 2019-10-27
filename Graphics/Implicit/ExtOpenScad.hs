-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: why are these required?
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- allow us to specify what package to import what module from.
-- We don't actually care, but when we compile our haskell examples, we do.
{-# LANGUAGE PackageImports #-}

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(String, Either(Left, Right), IO, ($), fmap, return)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)

import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, ScadOpts, Message(Message), MessageType(SyntaxError), CompState(CompState))

import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)

import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import Text.Parsec.Error (errorPos, errorMessages, showErrorMessages)

import Control.Monad (mapM_)

import "monads-tf" Control.Monad.State.Lazy (runStateT)

import System.Directory (getCurrentDirectory)

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: ScadOpts -> [String] -> String -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
runOpenscad scadOpts constants source =
    let
        rearrange :: (t, CompState) -> (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
        rearrange (_, CompState (varlookup, ovals, _, messages, _)) = (varlookup, obj2s, obj3s, messages) where
                                  (obj2s, obj3s, _) = divideObjs ovals
        show' err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
        mesg e = Message SyntaxError (sourcePosition $ errorPos e) $ show' e
    in do
      (initialObjects, initialMessages) <- addConstants constants
      case parseProgram "" source of
        Left e -> return (initialObjects, [], [], mesg e : initialMessages)
        Right sts -> fmap rearrange
            $ (\sts' -> do
                path <- getCurrentDirectory
                runStateT sts' $ CompState (initialObjects, [], path, initialMessages, scadOpts)
            )
            $ mapM_ runStatementI sts
