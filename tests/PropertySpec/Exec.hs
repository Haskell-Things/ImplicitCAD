{- ORMOLU_DISABLE -}
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
import           Prelude                                     (Bool (False), Floating, String, Double, Show,
                                                              Eq, Ord, fail, show,
                                                              ($), (&&), (+),
                                                              (.), (<$>), (<=), (*), (/),
                                                              (<>), (>=), (-))
import           Test.Hspec                                  (Spec, it)

approx :: (Floating a, Ord a) => a -> a -> a -> Bool
approx z a b = a + z >= b && a <= b + z

data Op = Add | Sub | Mul | Div
  deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

opName :: Op -> String 
opName Add = "addition"
opName Sub = "subttraction"
opName Mul = "multiplication"
opName Div = "division"

fromOp :: Op -> Double -> Double -> Double
fromOp Add = (+)
fromOp Sub = (-)
fromOp Mul = (*)
fromOp Div = (/)

mathsSpec :: Op -> Spec
mathsSpec o =
  it (opName o) . requireProperty $ do
    -- up to 100 values, between 1 and 1000
    l <- forAll . Gen.nonEmpty (Range.linear 1 100) . Gen.double $ Range.linearFrac 1 1000
    let e = fold . intersperse (show o) $ show <$> l
        n = foldl1 (fromOp o) l
    case (runExpr e False, num n) of
      ((ONum a, []), ONum b) -> diff a (approx 0.000001) b -- Some value to supress floating point inaccuracies
      (a, _)                 -> fail $ "Unexpected result value " <> show a

additionSpec :: Spec
additionSpec = mathsSpec Add

subtractionSpec :: Spec
subtractionSpec = mathsSpec Sub

multiplicationSpec :: Spec
multiplicationSpec = mathsSpec Mul

divisionSpec :: Spec
divisionSpec = mathsSpec Div
