-- Example 11 - the union of a square and a circle.
import Graphics.Implicit

out = union [
    squareR 0 True (80, 80)
  , translate (40, 40) $ circle 30
  ]

main = writeSVG 2 "example11.svg" out

