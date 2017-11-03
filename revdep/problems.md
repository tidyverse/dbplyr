# bigrquery

Version: 0.4.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5. Failure: DBItest: Connection: data_type_connection 
      6. Failure: DBItest: Connection: data_type_connection 
      7. Failure: DBItest: Connection: data_type_connection 
      8. Failure: DBItest: Result: send_query_invalid_connection 
      9. Failure: DBItest: Result: fetch_n_bad 
      1. ...
      
      Error: testthat unit tests failed
      In addition: Warning messages:
      1: Unused skip expressions: ^command_query$, ^data_logical_int.*, ^data_character_null_(above|below)$ 
      2: Unused skip expressions: ^roundtrip_logical_int$, ^roundtrip_64_bit$, ^quote_identifier_not_vectorized$ 
      3: Unused skip expressions: rows_affected 
      4: Unused skip expressions: read_only 
      Execution halted
      Deleting test2668
    ```

# dexter

Version: 0.5.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 109 marked UTF-8 strings
    ```

# dplyr

Version: 0.7.4

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

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

# implyr

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          intersect, setdiff, setequal, union
      
      > library(RJDBC)
      Loading required package: DBI
      Loading required package: rJava
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library/implyr/rJava/libs/rJava.so':
        dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library/implyr/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
        Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      Execution halted
    ```

# macleish

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DBI’
      All declared Imports should be used.
    ```

# mdsr

Version: 0.1.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2698 marked UTF-8 strings
    ```

# metagenomeFeatures

Version: 1.8.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        extdata   6.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .mgDb_annotateFeatures: no visible binding for global variable
      ‘db_keys’
    .select.taxa: no visible binding for global variable ‘Keys’
    .select.taxa: no visible binding for global variable ‘.’
    aggregate_taxa: no visible binding for global variable ‘.’
    aggregate_taxa: no visible binding for global variable ‘index’
    vignette_pheno_data: no visible global function definition for
      ‘read.csv’
    Undefined global functions or variables:
      . Keys db_keys index read.csv
    Consider adding
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# MonetDBLite

Version: 0.4.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      2: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      3: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      4: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      5: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      6: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      7: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      8: In .Internal(gc(verbose, reset)) :
        Connection is garbage-collected, use dbDisconnect() to avoid this.
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
    ```

# Organism.dplyr

Version: 1.2.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 134-151 (Organism.dplyr.Rmd) 
    Error: processing vignette 'Organism.dplyr.Rmd' failed with diagnostics:
    no applicable method for 'groups' applied to an object of class "c('sql', 'character')"
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

# replyr

Version: 0.8.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 151-159 (DependencySorting.Rmd) 
    Error: processing vignette 'DependencySorting.Rmd' failed with diagnostics:
    missing value where TRUE/FALSE needed
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RSQLite’ ‘dbplyr’
      All declared Imports should be used.
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
# sergeant

Version: 0.5.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: httr::POST(sprintf("%s/query.json", drill_server), encode = "json", body = list(queryType = "SQL", 
             query = query))
      3: request_perform(req, hu$handle$handle)
      4: request_fetch(req$output, req$url, handle)
      5: request_fetch.write_memory(req$output, req$url, handle)
      6: curl::curl_fetch_memory(url, handle = handle)
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 3
      1. Error: Core dbplyr ops work (@test-sergeant.R#12) 
      2. Failure: REST API works (@test-sergeant.R#25) 
      3. Error: REST API works (@test-sergeant.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# taxizedb

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      [31m1. Failure: db_load fails as expected - more (@test-db_load.R#33) [39m--------------
      suppressMessages(db_load_col(path = f)) did not throw an error.
      
      
      [31m2. Failure: src fails well (@test-src.R#16) [39m------------------------------------
      src_col() did not throw an error.
      
      
      testthat results ================================================================
      OK: 58 SKIPPED: 0 FAILED: 2
      1. Failure: db_load fails as expected - more (@test-db_load.R#33) 
      2. Failure: src fails well (@test-src.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

