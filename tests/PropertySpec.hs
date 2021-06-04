{- ORMOLU_DISABLE -}
module PropertySpec
  ( propSpec
  ) where

import Test.Hspec (Spec)
import PropertySpec.Exec (additionSpec, subtractionSpec, multiplicationSpec, divisionSpec)

propSpec :: Spec 
propSpec = do
  additionSpec
  subtractionSpec
  multiplicationSpec
  divisionSpec
