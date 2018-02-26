## Release summary

This is a minor release designed to add forward compability with the next version of rlang.

## Test environments
* OS X install: R 3.4
* win-builder: R-devel
* travis-ci: R 3.2, R 3.3, R 3.4, R-devel

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
