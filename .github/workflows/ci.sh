#!/usr/bin/env bash

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"

echo "regenerating .github/workflows/ci.yaml..."

# based on https://github.com/vmchale/github-actions-dhall
test -x $HOME/.cabal/bin/yaml-to-dhall || cabal install dhall-yaml
dhall-to-yaml --file ci.dhall > ci.yaml
