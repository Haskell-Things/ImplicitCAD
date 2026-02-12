{- ORMOLU_DISABLE -}
{- ImplicitCAD.
 - Copyright 2020-2026 Sandy McGuire, Julia Longtin
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- Property test container. tests basic operations. -}

module PropertySpec.Exec
  ( additionSpec
  , subtractionSpec
  , multiplicationSpec
  , divisionSpec
  , scadSpec  
  )where


import Prelude (Bool (True,False), Floating, String, Double, Show, Eq, Ord, fail, show, ($), (&&), (+), (.), (<$>), (<=), (*), (/), (<>), (>=), (-))

-- The Fold Library
import Data.Foldable (fold, foldl1)

-- For interspersing operators between numbers
import Data.List.NonEmpty (intersperse)

import ExecSpec.Util (num)

-- The values we are working with.
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))

-- To execute an expression.
import Graphics.Implicit.ExtOpenScad.Eval.Constant (runExpr)

import HaskellWorks.Hspec.Hedgehog (requireProperty)

import Hedgehog (diff, forAll)

import qualified Hedgehog.Gen as Gen

import qualified Hedgehog.Range as Range

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Test.QuickCheck.Property (label, liftBool, Property)

-- for boiling types down to their basics.
import Data.Coerce (coerce)

-- The numeric type in ImplicitCAD.
import Graphics.Implicit (ℝ)

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

-- I am given two boxes, with a distance between them greater than the size of either box. My task is to place a box in-between these two boxes.
prop_threeBoxes :: ℝ -> Property
prop_threeBoxes val = label ("hi!\nValue: " <> show val <> "\n") $ liftBool True

scadSpec :: Spec
scadSpec = do
  describe "Scad Generation" $ do
    it "generates our first scad code"
       $ property prop_threeBoxes
