## Test environments

* local: darwin15.6.0-3.5.1
* travis: 3.1, 3.2, 3.3, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results

0 errors | 0 warnings | 1 notes

* Missing or unexported object: 'dplyr::group_rows'
  This is only used for dplyr > 0.7.9, where it exists.

## revdepcheck results

We checked 47 reverse dependencies (42 from CRAN + 5 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 5 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* cytominer
  checking tests ...

* dplyr.teradata
  checking tests ...

* MonetDBLite
  checking tests ...

* pivot
  checking examples ... ERROR
  checking tests ...

* RPresto
  checking tests ...
  
These are legitimate failures due to deliberate interface changes (in order to uncover bugs), or fragile unit tests. All maintainers were informed about the release one week and one month ago.

### Failed to check

* arkdb
* dlookr
* dplyr 
