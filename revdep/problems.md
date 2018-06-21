# bigrquery

Version: 0.4.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m3. Error: date/times can be round-tripped (@test-upload.r#12) [39m [31m──────────────────────────────────────────[39m
      Billing has not been enabled for this project. Enable billing at https://console.cloud.google.com/billing.
      1: insert_upload_job("bigrquery-examples", "test", "x", df1) at testthat/test-upload.r:12
      2: bq_upload(url, c(config_part, data_part))
      3: process_request(req)
      4: signal_reason(out$error$errors[[1L]]$reason, out$error$message)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════
      OK: 55 SKIPPED: 0 FAILED: 3
      1. Error: (unknown) (@test-DBItest.R#2) 
      2. Error: extra arguments passed onto request (@test-datasets.r#6) 
      3. Error: date/times can be round-tripped (@test-upload.r#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# BiocFileCache

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: dbplyr
    Error: processing vignette 'BiocFileCache.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# dbplot

Version: 0.2.0

## In both

*   checking whether package ‘dbplot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘rlang’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/dbplot/new/dbplot.Rcheck/00install.out’ for details.
    ```

# dexter

Version: 0.6.0

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

# dplyr.teradata

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bit64’
      All declared Imports should be used.
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

# hydrolinks

Version: 0.7.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# implyr

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          intersect, setdiff, setequal, union
      
      > library(RJDBC)
      Loading required package: rJava
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so':
        dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
        Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      In addition: Warning message:
      package 'RJDBC' was built under R version 3.4.3 
      Execution halted
    ```

# mdsr

Version: 0.1.5

## In both

*   checking whether package ‘mdsr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.4.3
      Warning: package ‘mosaic’ was built under R version 3.4.3
      Warning: package ‘ggformula’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/mdsr/new/mdsr.Rcheck/00install.out’ for details.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    Warning: package ‘sp’ was built under R version 3.4.3
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2694 marked UTF-8 strings
    ```

# metagenomeFeatures

Version: 1.8.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data      1.0Mb
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

# mlbgameday

Version: 0.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in findCenvVar(name, cntxt$env) :
      closing unused connection 6 (http://gd2.mlb.com/components/game/mlb//year_2016/month_04/day_21/gid_2016_04_21_seamlb_clemlb_1/inning/inning_all.xml)
    Warning in findCenvVar(name, cntxt$env) :
      closing unused connection 5 (http://gd2.mlb.com/components/game/mlb//year_2016/month_04/day_21/gid_2016_04_21_lanmlb_atlmlb_1/inning/inning_all.xml)
    Quitting from lines 22-32 (pitch_plotting.Rmd) 
    Error: processing vignette 'pitch_plotting.Rmd' failed with diagnostics:
    `by` can't contain join column `batter` which is missing from LHS
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘iterators’ ‘parallel’
      All declared Imports should be used.
    ```

# MonetDBLite

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# mudata2

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hms’ ‘methods’
      All declared Imports should be used.
    ```

# Organism.dplyr

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 134-151 (Organism.dplyr.Rmd) 
    Error: processing vignette 'Organism.dplyr.Rmd' failed with diagnostics:
    no applicable method for 'groups' applied to an object of class "c('sql', 'character')"
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

# parsemsf

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# replyr

Version: 0.9.1

## In both

*   checking whether package ‘replyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘seplyr’ was built under R version 3.4.3
      Warning: package ‘wrapr’ was built under R version 3.4.3
      Warning: package ‘cdata’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/replyr/new/replyr.Rcheck/00install.out’ for details.
    ```

# RSQLServer

Version: 0.3.0

## In both

*   checking whether package ‘RSQLServer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/RSQLServer/new/RSQLServer.Rcheck/00install.out’ for details.
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
  error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RSQLServer’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/RSQLServer/new/RSQLServer.Rcheck/RSQLServer’

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
  error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/RSQLServer/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RSQLServer’
* removing ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/RSQLServer/old/RSQLServer.Rcheck/RSQLServer’

```
# seplyr

Version: 0.5.3

## In both

*   checking whether package ‘seplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘wrapr’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/seplyr/new/seplyr.Rcheck/00install.out’ for details.
    ```

# tidypredict

Version: 0.1.0

## In both

*   checking whether package ‘tidypredict’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘rlang’ was built under R version 3.4.3
      Warning: package ‘tibble’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/tidypredict/new/tidypredict.Rcheck/00install.out’ for details.
    ```

# tidyverse

Version: 1.2.1

## In both

*   checking whether package ‘tidyverse’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/tidyverse/new/tidyverse.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

