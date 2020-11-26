module GoldenSpec.Spec (spec) where

import GoldenSpec.Util (golden)
import Graphics.Implicit
import Prelude (($), Bool (True))
import Test.Hspec ( describe, Spec )


spec :: Spec
spec = describe "golden tests" $ do
  golden "box" 1 $ cubeR 0 True (5, 5, 5)

