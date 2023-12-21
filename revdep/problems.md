# CDMConnector

<details>

* Version: 1.1.4
* GitHub: https://github.com/darwin-eu/CDMConnector
* Source code: https://github.com/cran/CDMConnector
* Date/Publication: 2023-10-20 21:50:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "CDMConnector")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       20.     │   └─base (local) doWithOneRestart(return(expr), restart)
       21.     └─vctrs::stop_incompatible_cast(...)
       22.       └─vctrs::stop_incompatible_type(...)
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 5 | WARN 0 | SKIP 15 | PASS 167 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_getting-started.Rmd’ using rmarkdown
    trying URL 'https://example-data.ohdsi.dev/GiBleed.zip'
    Content type 'application/zip' length 6754786 bytes (6.4 MB)
    ==================================================
    downloaded 6.4 MB
    
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    --- finished re-building ‘a01_getting-started.Rmd’
    ...
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘a06_using_cdm_attributes.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following file failed:
      ‘a06_using_cdm_attributes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'CirceR', 'Capr'
    ```

# DrugUtilisation

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/DrugUtilisation
* Date/Publication: 2023-10-20 04:40:06 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "DrugUtilisation")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       20.     │   └─base (local) doWithOneRestart(return(expr), restart)
       21.     └─vctrs::stop_incompatible_cast(...)
       22.       └─vctrs::stop_incompatible_type(...)
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 8 | WARN 0 | SKIP 18 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Intro_create_cohort.Rmd’ using rmarkdown
    
    Quitting from lines 23-38 [setup] (Intro_create_cohort.Rmd)
    Error: processing vignette 'Intro_create_cohort.Rmd' failed with diagnostics:
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘Intro_create_cohort.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ...
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    --- finished re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Intro_create_cohort.Rmd’ ‘addIndications-example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# editbl

<details>

* Version: 1.0.1
* GitHub: https://github.com/openanalytics/editbl
* Source code: https://github.com/cran/editbl
* Date/Publication: 2023-10-13 15:20:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "editbl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘editbl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: e_rows_insert.tbl_dbi
    > ### Title: rows_insert implementation for DBI backends.
    > ### Aliases: e_rows_insert.tbl_dbi
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > # Insert new row
    > artists <- tbl(conn, "Artist")
    > DBI::dbBegin(conn)
    > e_rows_insert(artists,
    +  data.frame(ArtistId = 999, Name = "testArtist"),
    +  in_place = TRUE)
    Error in initialize(value, ...) : 
      cannot use object of class “dbplyr_table_ident” in new():  class “SQL” does not extend that class
    Calls: e_rows_insert ... get_db_table_name -> <Anonymous> -> new -> initialize -> initialize
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(editbl)
      > 
      > test_check("editbl")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 55 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       16.   ├─.transformer(expr, env) %||% .null
       17.   └─glue (local) .transformer(expr, env)
       18.     └─base::eval(parse(text = text, keep.source = FALSE), envir)
       19.       └─base::eval(parse(text = text, keep.source = FALSE), envir)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 55 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# IncidencePrevalence

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/IncidencePrevalence
* Date/Publication: 2023-10-18 20:30:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "IncidencePrevalence")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IncidencePrevalence-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mockIncidencePrevalenceRef
    > ### Title: Generate example subset of the OMOP CDM for estimating incidence
    > ###   and prevalence
    > ### Aliases: mockIncidencePrevalenceRef
    > 
    > ### ** Examples
    > 
    ...
     19.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     20.     │   └─base (local) doWithOneRestart(return(expr), restart)
     21.     └─vctrs::stop_incompatible_cast(...)
     22.       └─vctrs::stop_incompatible_type(...)
     23.         └─vctrs:::stop_incompatible(...)
     24.           └─vctrs:::stop_vctrs(...)
     25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(IncidencePrevalence)
      > 
      > test_check("IncidencePrevalence")
      Starting 2 test processes
      [ FAIL 17 | WARN 0 | SKIP 76 | PASS 81 ]
      
    ...
       20.     │   └─base (local) doWithOneRestart(return(expr), restart)
       21.     └─vctrs::stop_incompatible_cast(...)
       22.       └─vctrs::stop_incompatible_type(...)
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 17 | WARN 0 | SKIP 76 | PASS 81 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’ using rmarkdown
    
    Quitting from lines 54-58 [unnamed-chunk-4] (a01_Introduction_to_IncidencePrevalence.Rmd)
    Error: processing vignette 'a01_Introduction_to_IncidencePrevalence.Rmd' failed with diagnostics:
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ...
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘a01_Introduction_to_IncidencePrevalence.Rmd’
      ‘a02_Creating_denominator_populations.Rmd’
      ‘a04_Calculating_prevalence.Rmd’ ‘a05_Calculating_incidence.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mlr3db

<details>

* Version: 0.5.1
* GitHub: https://github.com/mlr-org/mlr3db
* Source code: https://github.com/cran/mlr3db
* Date/Publication: 2023-10-17 08:40:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "mlr3db")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library("testthat")
      +   library("mlr3db")
      +   test_check("mlr3db")
      + }
      Loading required package: mlr3
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1021 ]
    ...
       25.         │ └─base::lapply(...)
       26.         ├─private$.data$ops
       27.         └─dbplyr:::`$.tbl_lazy`(private$.data, "ops")
       28.           └─cli::cli_abort(...)
       29.             └─rlang::abort(...)
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1021 ]
      Error: Test failures
      In addition: There were 27 warnings (use warnings() to see them)
      Execution halted
    ```

# modeldb

<details>

* Version: 0.2.3
* GitHub: https://github.com/tidymodels/modeldb
* Source code: https://github.com/cran/modeldb
* Date/Publication: 2022-08-16 20:30:15 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "modeldb")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘kmeans.Rmd’ using rmarkdown
    
    Quitting from lines 55-56 [unnamed-chunk-3] (kmeans.Rmd)
    Error: processing vignette 'kmeans.Rmd' failed with diagnostics:
    The `$` method of <tbl_lazy> is for internal use only.
    ℹ Use `dplyr::pull()` to get the values in a column.
    --- failed re-building ‘kmeans.Rmd’
    
    --- re-building ‘linear-regression.Rmd’ using rmarkdown
    --- finished re-building ‘linear-regression.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘kmeans.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PatientProfiles

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/PatientProfiles
* Date/Publication: 2023-10-06 15:00:05 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "PatientProfiles")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
       20.     │   └─base (local) doWithOneRestart(return(expr), restart)
       21.     └─vctrs::stop_incompatible_cast(...)
       22.       └─vctrs::stop_incompatible_type(...)
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 81 | WARN 16 | SKIP 0 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘addCohortIntersections.Rmd’ using rmarkdown
    
    Quitting from lines 47-55 [unnamed-chunk-2] (addCohortIntersections.Rmd)
    Error: processing vignette 'addCohortIntersections.Rmd' failed with diagnostics:
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘addCohortIntersections.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    ...
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘addPatientCharacteristics.rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘addCohortIntersections.Rmd’ ‘addPatientCharacteristics.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RPresto

<details>

* Version: 1.4.5
* GitHub: https://github.com/prestodb/RPresto
* Source code: https://github.com/cran/RPresto
* Date/Publication: 2023-05-05 08:20:19 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "RPresto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Copyright (c) Meta Platforms, Inc. and affiliates.
      > # All rights reserved.
      > #
      > # This source code is licensed under the BSD-style license found in the
      > # LICENSE file in the root directory of this source tree.
      > 
      > library("testthat")
    ...
        6.   └─base::tryCatch(...)
        7.     └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        8.       └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        9.         └─value[[3L]](cond)
       10.           └─cli::cli_abort("Can't query fields.", parent = cnd)
       11.             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 79 | PASS 210 ]
      Error: Test failures
      Execution halted
    ```

# sparklyr

<details>

* Version: 1.8.3
* GitHub: https://github.com/sparklyr/sparklyr
* Source code: https://github.com/cran/sparklyr
* Date/Publication: 2023-09-02 05:10:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "sparklyr")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    db_connection_describe:
      function(con, ...)
    db_connection_describe.src_spark:
      function(con)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        java   3.8Mb
    ```

