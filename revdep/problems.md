# grasp2db

Version: 1.0.0

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘GRASP2’ ‘checkAnti’ ‘getJoinCompatible’
    Undocumented data sets:
      ‘mml10p_nox’ ‘uniqueGexNames2.0’ ‘uniquePPDnames2.0’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... WARNING
    ```
      Warning: found non-ASCII string
      'Beh<e7>et's disease' in object 'uniquePPDnames2.0'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                            old_size new_size compress
      mml10p_nox.rda           7.1Mb    2.8Mb       xz
      uniquePPDnames2.0.rda     17Kb     15Kb    bzip2
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘AnnotationHubData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Artistic-2.0 + file LICENSE
    ```

*   checking R code for possible problems ... NOTE
    ```
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
    checkAnti: no visible binding for global variable ‘chr_hg19’
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# RSQLServer

Version: 0.3.0

## In both

*   checking whether package ‘RSQLServer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks/RSQLServer/new/RSQLServer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RSQLServer’ ...
** package ‘RSQLServer’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RSQLServer’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks/RSQLServer/new/RSQLServer.Rcheck/RSQLServer’

```
### CRAN

```
* installing *source* package ‘RSQLServer’ ...
** package ‘RSQLServer’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library/RSQLServer/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RSQLServer’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks/RSQLServer/old/RSQLServer.Rcheck/RSQLServer’

```
