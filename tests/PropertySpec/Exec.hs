module PropertySpec.Exec
  ( additionSpec
  , subtractionSpec
  , multiplicationSpec
  , divisionSpec
  )where

import           Data.Foldable                               (fold, foldl1)
import           Data.List.NonEmpty                          (intersperse)
import           ExecSpec.Util                               (num)
import           Graphics.Implicit.ExtOpenScad.Definitions   (OVal (ONum))
import           Graphics.Implicit.ExtOpenScad.Eval.Constant (runExpr)
import           HaskellWorks.Hspec.Hedgehog                 (requireProperty)
import           Hedgehog                                    (diff, forAll)
import qualified Hedgehog.Gen                                as Gen
import qualified Hedgehog.Range                              as Range
import           Prelude                                     (Bool, Floating, String, Double,
                                                              Ord, fail, show,
                                                              ($), (&&), (+),
                                                              (.), (<$>), (<=), (*), (/),
                                                              (<>), (>=), (-))
import           Test.Hspec                                  (Spec, it)

approx :: (Floating a, Ord a) => a -> a -> a -> Bool
approx z a b = a + z >= b && a <= b + z

mathsSpec :: (Double -> Double -> Double) -> String -> Spec
mathsSpec f s =
  it "addition" . requireProperty $ do
    l <- forAll . Gen.nonEmpty (Range.linear 1 100) . Gen.double $ Range.linearFrac 1 1000
    let e = fold . intersperse s $ show <$> l
        n = foldl1 f l
    case (runExpr e, num n) of
      ((ONum a, []), ONum b) -> diff a (approx 0.000001) b -- Some value to supress floating point inaccuracies
      (a, _)                 -> fail $ "Unexpected result value " <> show a

additionSpec :: Spec
additionSpec = mathsSpec (+) "+"

subtractionSpec :: Spec
subtractionSpec = mathsSpec (-) "-"

multiplicationSpec :: Spec
multiplicationSpec = mathsSpec (*) "*"

divisionSpec :: Spec
divisionSpec = mathsSpec (/) "/"
