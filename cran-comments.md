## Release summary

This is a minor release designed to add forward compability with the next version of rlang.

## Test environments

* local: darwin15.6.0-3.5.1
* travis: 3.1, 3.2, 3.3, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 41 reverse dependencies (37 from CRAN + 4 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* RSQLServer (failed to install) -this is a long-standing failure because
  I don't have java configured.
