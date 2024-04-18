-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(String, IO, ($), (<$>), pure, either, (.), Applicative, Bool(True), Maybe, maybe)

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)

import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, ScadOpts, Message(Message), MessageType(SyntaxError), CompState(CompState, scadVars, oVals), StatementI, runImplicitCadM)

import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)

import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)

import Text.Parsec.Error (errorPos, errorMessages, showErrorMessages, ParseError)

import System.Directory (getCurrentDirectory)

import Data.Foldable (traverse_)

import Data.Text.Lazy (pack)
import System.FilePath (FilePath, takeDirectory)

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: ScadOpts -> [String] -> Maybe FilePath -> String -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
runOpenscad scadOpts constants filepath source = do
  (initialObjects, initialMessages) <- addConstants constants True
  let
    err :: Applicative f => ParseError -> f (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    err e = pure (initialObjects, [], [], mesg e : initialMessages)
    run :: [StatementI] -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    run sts = rearrange <$> do
      let sts' = traverse_ runStatementI sts
      -- If we are given a filepath, use its directory, relative or absolute.
      -- If there is no filepath given, then use the current directory of the process.
      path <- maybe getCurrentDirectory (pure . takeDirectory) filepath
      let initState = CompState initialObjects [] path
      (_, w, s') <- runImplicitCadM scadOpts initState sts'
      pure (w, s')
  either err run $ parseProgram "" source
  where
    rearrange :: ([Message], CompState) -> (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    rearrange (messages,  s) =
      let (obj2s, obj3s, _) = divideObjs $ oVals s
      in (scadVars s, obj2s, obj3s, messages)
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    mesg e = Message SyntaxError (sourcePosition $ errorPos e) $ pack $ show' e
