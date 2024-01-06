{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.Implicit.Interpreter (
    ImplicitInterpreterError(..)
  , renderError
  , interpret
  , interpretText
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Typeable (Typeable)
import Data.Text (Text)
import Language.Haskell.Interpreter (InterpreterError(..), InterpreterT)

import qualified Control.Monad.IO.Class
import qualified Data.Foldable
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified Language.Haskell.Interpreter
import qualified System.FilePath
import qualified System.IO.Temp
import qualified Type.Reflection

data ImplicitInterpreterError
  = ImplicitInterpreterError_Unsafe -- ^ Thrown when module contains unsafe functions
  | ImplicitInterpreterError_Hint InterpreterError
  deriving Show

renderError :: ImplicitInterpreterError -> Text
renderError ImplicitInterpreterError_Unsafe = "Refusing to evaluate unsafe functions"
renderError (ImplicitInterpreterError_Hint e) = renderHintError e

renderHintError :: InterpreterError -> Text
renderHintError (WontCompile ghcErrs) =
  Data.Text.unlines
  $ Data.Text.pack
  . Language.Haskell.Interpreter.errMsg
  <$> ghcErrs
renderHintError (UnknownError str) = "Unknown error: " <> Data.Text.pack str
renderHintError (NotAllowed str) = "Not allowed: " <> Data.Text.pack str
renderHintError (GhcException str) = "GHC exception: " <> Data.Text.pack str

-- | Interpret a file, trying to evaluate @obj@ variable
-- representing @SymbolicObj2@ or @SymbolicObj3@.
--
-- If file defines resolution as @res@ variable,
-- evaluate it (if it is a Double, Float, Int or Integer)
-- and return alongside an evaluated object. Return
-- default resolution of 1 if not defined.
interpret
  :: forall a
   . Typeable a
  => FilePath
  -> IO (Either ImplicitInterpreterError (Double, a))
interpret modFile = do
  let
      -- we always eval obj variable
      exprToEval = "obj"
      initialResolution = 1

  mo <-
    Language.Haskell.Interpreter.runInterpreter
    $ do
        Language.Haskell.Interpreter.set
          [ Language.Haskell.Interpreter.searchPath
            Language.Haskell.Interpreter.:=
            pure (System.FilePath.takeDirectory modFile)
          ]

        Language.Haskell.Interpreter.loadModules
          [modFile]

        loadedMods
          <- Language.Haskell.Interpreter.getLoadedModules

        Language.Haskell.Interpreter.setTopLevelModules
          loadedMods

        obj <-
          Language.Haskell.Interpreter.interpret
            exprToEval
            (Language.Haskell.Interpreter.as @a)

        mRes <-
          runMaybeT
          $ Data.Foldable.asum
          $ map
              MaybeT
              [                       evalRes @Double
              , fmap realToFrac   <$> evalRes @Float
              , fmap fromIntegral <$> evalRes @Int
              , fmap fromIntegral <$> evalRes @Integer
              ]

        pure
          ( Data.Maybe.fromMaybe initialResolution mRes
          , obj
          )

  pure $ case mo of
    Right x -> Right x
    Left e -> Left $ ImplicitInterpreterError_Hint e

evalRes
  :: forall t m
   . ( MonadIO m
     , MonadMask m
     , Typeable t
     , Num t
     )
  => InterpreterT m (Maybe t)
evalRes = do
  tcs <-
    Language.Haskell.Interpreter.typeChecks
      (  "res :: "
      <> (show $ Type.Reflection.typeOf @t 0)
      )
  if tcs
  then
    Just
    <$> Language.Haskell.Interpreter.interpret
          "res"
          (Language.Haskell.Interpreter.as @t)
  else pure Nothing

-- | Interpret a text
-- representing @SymbolicObj2@ or @SymbolicObj3@.
--
-- Inpute can be either a full module or bare object
-- expression like @sphere 3@ in which case it is
-- wrapped in a module template using @makeModule@ with typical
-- imports and equated to @obj = sphere 3@.
--
-- Temporary directory is created with a @Object.hs@ file
-- which is then passed to @interpret@, which sets a search
-- path to the directory.
--
-- Since this is used by implicit-servant to evaluate
-- untrusted input, we refuse to evaluate anything
-- containing @unsafe@ string, which could be used
-- to perform @IO@ using e.g. @unsafePerformIO@
-- during @obj@ evaluation which is otherwise pure.
interpretText
  :: forall a
   . Typeable a
  => Text
  -> IO (Either ImplicitInterpreterError (Double, a))
interpretText code | isUnsafe (describeInput code) =
  pure $ Left ImplicitInterpreterError_Unsafe
interpretText code =
  let
    InputDesc{..} = describeInput code
    codeModule =
      if hasImports && hasModule
      then code
      else makeModule code
  in
    withModuleAsFile
      codeModule
      interpret

withModuleAsFile
  :: ( MonadMask m
     , MonadIO m
     )
  => Text
  -> (FilePath -> m a)
  -> m a
withModuleAsFile source action = do
  System.IO.Temp.withSystemTempDirectory
    "implicitInterpreter"
    $ \dir -> do
        let
          fp =
            System.FilePath.joinPath
              [ dir
              , "Object.hs"
              ]

        Control.Monad.IO.Class.liftIO
          $ Data.Text.IO.writeFile
              fp
              source

        action fp

-- | Input description
data InputDesc = InputDesc {
    hasImports :: Bool -- ^ Contains import statements
  , hasModule :: Bool -- ^ Contains module definition
  , hasPragmas :: Bool -- ^ Contains LANGAUGE pragmas
  , isUnsafe :: Bool -- ^ Contains any @unsafe@ functions
  } deriving (Eq, Show)

-- | Check if input is a proper module, with
-- imports and detect any @unsafe@ functions.
describeInput :: Text -> InputDesc
describeInput src =
  let
    has what x = what `Data.Text.isInfixOf` x
    hasImports = has "import" src
    hasModule = has "module" src
    hasPragmas = has "LANGUAGE" src
    isUnsafe = has "unsafe" src
  in InputDesc {..}

-- describeInput "module Blah where\nimport Foo\nunsafePerform\nLANGUAGE"
-- InputDesc {hasImports = True, hasModule = True, hasPragmas = True, isUnsafe = True}

makeModule
  :: Text -- ^ Input sauce
  -> Text
makeModule rawExpr =
  Data.Text.unlines
    [ "module Object (obj) where"
    , mempty
    , "import Linear"
    , "import Graphics.Implicit"
    , "import Graphics.Implicit.Definitions"
    , mempty
    , "obj = " <> rawExpr
    ]
