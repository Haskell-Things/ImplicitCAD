{- ORMOLU_DISABLE -}
module Graphics.Implicit.Test.Utils (randomGroups) where

import Prelude (drop, (<*>), (<$>), take, length, pure)
import Test.QuickCheck ( choose, Gen )

{-# ANN randomGroups "HLint: ignore Redundant <$>" #-}
randomGroups :: [a] -> Gen [[a]]
randomGroups [] = pure []
randomGroups as = do
  n <- choose (1, length as)
  (:) <$> pure (take n as)
      <*> randomGroups (drop n as)

