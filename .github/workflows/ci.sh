#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail

# cd into the dir where this script is placed
cd "$( dirname "${BASH_SOURCE[0]}" )"

echo "regenerating .github/workflows/ci.yaml"

which dhall-to-yaml-ng || cabal install dhall-yaml
dhall-to-yaml-ng --generated-comment --file ci.dhall > ci.yaml
