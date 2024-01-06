{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Graphics.Implicit (SymbolicObj3, sphere)
import Graphics.Implicit.Canon (EqObj((=^=)))
import Graphics.Implicit.Interpreter
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , hspec
  , it
  , shouldBe
  , shouldSatisfy
  )

import qualified Data.Either
import qualified Data.Text

spec :: Spec
spec = do
  describe "interpretText" $ do
    it "interprets plain sphere" $ do
      interpretsFine
        "sphere 3"
        (sphere 3)
        1

    it "interprets sphere module" $ do
      interpretsFine
        sphereMod
        (sphere 3)
        2

    it "refuses unsafe" $ do
      res <-
        interpretText
          @SymbolicObj3
          ("unsafePerformIO $ cube")
      res `shouldSatisfy` Data.Either.isLeft

interpretsFine
  :: ( EqObj a
     , Show a
     , Typeable a
     )
  => Text
  -> a
  -> Double
  -> IO ()
interpretsFine input object resolution =
  interpretText
    input
  >>= \case
    Left e ->
      expectationFailure
        $  "Interpreter failed with "
        <> show e
    Right (res, obj) -> do
      res `shouldBe` resolution
      obj `shouldSatisfy` (=^= object)

sphereMod :: Text
sphereMod =
  Data.Text.unlines
    [ "module Obj (obj) where"
    , "import Graphics.Implicit"
    , "obj = sphere 3"
    , "res = 2"
    ]

main :: IO ()
main = hspec spec
