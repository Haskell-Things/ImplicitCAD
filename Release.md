# Release Processes:

Purpose of this document: to make sure we follow a consistent pattern, when making releases of ImplicitCAD.

## Version Logic:
1. The first digit is always 0. Maybe we'll change this when we're ready for the masses. ;)
2. The second digit changes with "major" releases.
 * Major releases change:
   * the CSG representation (the MD5sum of generated output files)
   * the Haskell interface (in a non-additive fashion)
   * or the SCAD interface (in a non-additive fashion)
3. The third digit changes with the "minor" releases.
 * Minor releases DO NOT change:
   * the CSG representation (the MD5sum of generated output files)
   * the Haskell interface (in a non-additive fashion)
   * or the SCAD interface (in a non-additive fashion)
 * Minor releases may change anything else.
4. The fourth digit changes with the "trivial" releases.
 * Trivial releases change nothing except the documentation.

## Tests for a Minor Release
1. make sure the output of the test-implicit binary is all green.
2. make sure the output of the docgen executable hasn't changed too greatly since the last release.
  * run `make docs`
  * examine the difference in the docs/escad.md
3. make sure the output of the parser-bench binary does not show any unacceptable speed reductions.
4. make sure the output of the Benchmark binary does not show any unacceptable speed reductions.
5. make sure the md5sum of the stl files resulting from running 'make examples' have not changed.

## Performing a release

### Create a Release branch
1. `git checkout -b release/<VERSION>`
2. update the Version field in implicit.cabal.
3. update the Version in the README.md.
4. change the most recent Version line in CHANGELOG.md from 'next', updating the following fields on that line.
5. push the branch to github, and file a pull request.

### Merge to master
In the github interface, after all of the tests are green, merge to the master branch.

### Tagging a release
On your git machine:
```
export VERSION=<VERSION_NUMBER>
git checkout master
git tag -a v$VERSION -m "Release $VERSION"
git push origin v$VERSION
```

### Publishing the release to GitHub

1. Open Github.
2. Click on the 'Releases' link from the code page for the implicitcad repo.
3. Click on 'Draft a new release'
4. Select the tag created in the previous step.
5. Paste the CHANGELOG.md entries from this release into the release description.
6. Title the release 'Release <versionnumber>'
7. Click on 'Publish release'

### Publishing the release to Hackage

1. Use github's 'download zip' to download a zip of the package.
2. Extract it to a temporary directory
3. Move the container directory to `implicit-<VERSION>`
4. Make a tar file from it. make sure to add the --format=ustar option.
 * `tar --format=ustar -cvzf implicit-<VERSION>.tar.gz implicit-<VERSION>/`
5. Upload the package candidate to https://hackage.haskell.org/packages/candidates/upload
6. Look over the resulting page.
7. Scroll down to 'edit package information'
8. click on 'publish candidate'
9. hit the 'publish package' button.

### Update ImplicitCAD.org
1. Use the output of docgen to update implicitcad.org (FIXME: how?)

### Re-Anchor the ChangeLog.
File a new PR for adding a clean '# Version [next](https://github.com/Haskell-Things/ImplicitCAD/compare/v0.4.0.0...master) (202Y-MM-DD)' to the top of the Changelog, with a single empty bullet point.
