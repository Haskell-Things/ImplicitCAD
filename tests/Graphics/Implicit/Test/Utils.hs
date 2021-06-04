{- ORMOLU_DISABLE -}
module Graphics.Implicit.Test.Utils where

import Prelude (drop, (<*>), (<$>), take, length, pure)
import Test.QuickCheck ( choose, Gen )

randomGroups :: [a] -> Gen [[a]]
randomGroups [] = pure []
randomGroups as = do
  n <- choose (1, length as)
  (:) <$> pure (take n as)
      <*> randomGroups (drop n as)

