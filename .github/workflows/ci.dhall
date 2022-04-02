let haskellCi = https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC902
              , haskellCi.GHC.GHC8107
              , haskellCi.GHC.GHC884
              , haskellCi.GHC.GHC865
              ]
            , cabal = [ haskellCi.Cabal.Cabal34 ]
            }
        )
        // { on = [ haskellCi.Event.push
                  , haskellCi.Event.pull_request ]
           }
    : haskellCi.CI.Type
