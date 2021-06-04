{- ORMOLU_DISABLE -}
-- Example 11 - the union of a square and a circle.
import Graphics.Implicit

out = union [
    square True (V2 80 80)
  , translate (V2 40 40) $ circle 30
  ]

main = writeSVG 2 "example11.svg" out

