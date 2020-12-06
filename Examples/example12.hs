-- Example 12 - the rounded union of a square and a circle.
import Graphics.Implicit

out = unionR 14 [
    squareR 0 (80, 80)
  , translate (40,40) $ circle 30
  ]

main = writeSVG 2 "example12.svg" out
