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