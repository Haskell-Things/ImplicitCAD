{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Utilities
module MessageSpec.Util
       ( (-->)
       , oneMessage
       ) where

-- be explicit about where we get things from.
import Prelude (String, Bool(False), IO, return)

-- Expressions, symbols, and values in the OpenScad language.
import Graphics.Implicit.ExtOpenScad.Definitions (ScadOpts(ScadOpts), MessageType, Message(Message), SourcePosition)

import Graphics.Implicit.ExtOpenScad (runOpenscad)

import Test.Hspec (Expectation, shouldReturn)

import Data.Text.Lazy (Text)

-- | decide what options to send to the scad engine.
generateScadOpts :: ScadOpts
generateScadOpts = ScadOpts compat_flag import_flag
  where
    compat_flag = False -- Do not try to be extra compatible with openscad.
    import_flag = False -- Do not honor include or use statements.

-- An operator for expressions for "the left side should evaluate to the right side."
infixr 1 -->
(-->) :: String -> [Message] -> Expectation
(-->) source value =
  getOpenscadMessages scadOptions [] source `shouldReturn` value
  where
    scadOptions = generateScadOpts

-- | An even smaller wrapper which runs a program, and only returns the generated messages. for the test suite.
getOpenscadMessages ::  ScadOpts -> [String] -> String -> IO [Message]
getOpenscadMessages scadOpts constants source = do
    (_, _, _, messages) <- runOpenscad scadOpts constants source
    return messages

oneMessage :: MessageType -> SourcePosition -> Text -> [Message]
oneMessage msgType pos text = [Message msgType pos text]
