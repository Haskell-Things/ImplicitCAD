{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Prelude(String, IO, ($), (<$>), pure, either, (.), Applicative, Bool(True))

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

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: ScadOpts -> [String] -> String -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
runOpenscad scadOpts constants source = do
  (initialObjects, initialMessages) <- addConstants constants True
  let
    err :: Applicative f => ParseError -> f (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    err e = pure (initialObjects, [], [], mesg e : initialMessages)
    run :: [StatementI] -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    run sts = rearrange <$> do
      let sts' = traverse_ runStatementI sts
      path <- getCurrentDirectory
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
