# Release Processes:

Purpose of this document: to make sure i follow a consistent patern, when making changes to ImplicitCAD.

## "no point" releases:

### Comment / Format / Messages

These changes don't improve anything but the code quality, messages output, or build system. they can add features to the parser, but cannot remove them. they may not change the md5 of the generated STL files.

1. make sure test-implicit is all green.
2. make sure docgen hasn't changed it's output too much.
3. make sure parser-bench does not show any unacceptable speed reductions.
4. make sure Benchmark does not show any unacceptable speed reductions.
5. make sure the md5sum of the stl files resulting from running 'make examples' have not changed.

push to master.

### Math / Types
These releases change the math engine, but only in a direction that is provably better, and shows in our examples.

1. do all of the above.
2. check 'make examples' output. look at the times that valgrind measures.
3. check the md5sum of the .stl files output.

If the md5sums of the last release and this one differ, run admesh on both, and examine the output. if the output is conclusively better for all changed examples, then proceed to push.

push to master.

## point releases:

These releases change the quality of the output significantly enough that poking it with admesh is indeterminate, or they include changes to the parser such that old code would not work.

## major releases:

These bring new features, and other improvements that are considered to be 'major'.

### Process:
merge the release PR in github.

on your git machine:
 * make sure 'cabal haddock' works.



#### Tagging a release
on your git machine:
```
git checkout master
git tag -a v0.4.0.0 -m 'release 0.4.0.0'
git push origin v0.4.0.0
```

#### Publishing the release to hackage

use github's 'download zip' to download a zip of the package.
extract it to a temporary directory
move the container directory to implicit-<VERSIONNUMBER>
make a tar file from it. make sure to add the --format=ustar option.
```
tar --format=ustar -cvzf implicit-0.4.0.0.tar.gz implicit-0.4.0.0/
```
upload package candidate to https://hackage.haskell.org/packages/candidates/upload
look over the resulting page.

scroll down to 'edit package information'

click on 'publish candidate'

hit the 'publish package' button.

#### Publishing the release to GitHub

open github.
hit the 'Releases' link from the code page for the repo.
hit 'draft a new release
select the tag.
paste the CHANGELOG entries in the description.
title the release 'Release <versionnumber>'
hit 'Publish release'

