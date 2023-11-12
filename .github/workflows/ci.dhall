let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

in    haskellCi.generalCi
        (haskellCi.withHlint haskellCi.defaultCabalSteps)
        (haskellCi.DhallMatrix::{=} with ghc = haskellCi.defaultGHC3)
    : haskellCi.CI.Type
