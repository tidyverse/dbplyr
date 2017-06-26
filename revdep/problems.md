# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.4.0 Patched (2017-06-13 r72789) |
|system   |x86_64, darwin15.6.0                        |
|ui       |RStudio (1.1.271)                           |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Chicago                             |
|date     |2017-06-26                                  |

## Packages

|package      |*  |version    |date       |source                           |
|:------------|:--|:----------|:----------|:--------------------------------|
|covr         |   |2.2.2      |2017-01-05 |cran (@2.2.2)                    |
|DBI          |*  |0.7        |2017-06-18 |cran (@0.7)                      |
|dbplyr       |   |1.1.0      |2017-06-26 |local (tidyverse/dbplyr@NA)      |
|knitr        |   |1.16       |2017-05-18 |cran (@1.16)                     |
|Lahman       |   |5.0-0      |2016-08-27 |cran (@5.0-0)                    |
|nycflights13 |   |0.2.2      |2017-01-27 |cran (@0.2.2)                    |
|purrr        |   |0.2.2.9000 |2017-06-26 |Github (tidyverse/purrr@6dda577) |
|R6           |   |2.2.2      |2017-06-17 |cran (@2.2.2)                    |
|rmarkdown    |   |1.6        |2017-06-15 |cran (@1.6)                      |
|RMySQL       |   |0.10.11    |2017-03-29 |cran (@0.10.11)                  |
|RPostgreSQL  |   |0.6-2      |2017-06-24 |CRAN (R 3.4.0)                   |
|RSQLite      |   |1.1-2      |2017-01-08 |cran (@1.1-2)                    |
|testthat     |   |1.0.2      |2016-04-23 |cran (@1.0.2)                    |

# Check results
3 packages with problems

## dplyr (0.7.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/tidyverse/dplyr/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [11s/11s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  4: bit64::as.integer64
  5: getExportedValue(pkg, name)
  6: asNamespace(ns)
  7: getNamespace(ns)
  8: tryCatch(loadNamespace(name), error = function(e) stop(e))
  9: tryCatchList(expr, classes, parentenv, handlers)
  10: tryCatchOne(expr, names, parentenv, handlers[[1L]])
  11: value[[3L]](cond)
  
  testthat results ================================================================
  OK: 2658 SKIPPED: 7 FAILED: 1
  1. Error: combine works with integer64 (#1092) (@test-combine.R#174) 
  
  Error: testthat unit tests failed
  Execution halted
```

## implyr (0.2.0)
Maintainer: Ian Cook <ian@cloudera.com>  
Bug reports: https://github.com/ianmcook/implyr/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
      intersect, setdiff, setequal, union
  
  > library(RJDBC)
  Loading required package: DBI
  Loading required package: rJava
  Error: package or namespace load failed for 'rJava':
   .onLoad failed in loadNamespace() for 'rJava', details:
    call: dyn.load(file, DLLpath = DLLpath, ...)
    error: unable to load shared object '/Users/hadley/R-revdep/rJava/libs/rJava.so':
    dlopen(/Users/hadley/R-revdep/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
    Referenced from: /Users/hadley/R-revdep/rJava/libs/rJava.so
    Reason: image not found
  Error: package 'rJava' could not be loaded
  Execution halted
```

## RSQLServer (0.3.0)
Maintainer: Imanuel Costigan <i.costigan@me.com>  
Bug reports: https://github.com/imanuelcostigan/RSQLServer/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘RSQLServer’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/dplyr/dbplyr/revdep/checks/RSQLServer.Rcheck/00install.out’ for details.
```

