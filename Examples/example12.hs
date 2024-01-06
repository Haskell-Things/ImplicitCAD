-- Example 12 - the rounded union of a square and a circle.
import Control.Applicative (pure)
import Graphics.Implicit

out = unionR 14 [
    square True (pure 80) -- pure 80 turns into (V2 80 80)
  , translate (pure 40) $ circle 30
  ]

main = writeSVG 2 "example12.svg" out
