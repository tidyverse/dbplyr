# CDMConnector

<details>

* Version: 1.1.2
* GitHub: https://github.com/darwin-eu/CDMConnector
* Source code: https://github.com/cran/CDMConnector
* Date/Publication: 2023-08-23 00:20:06 UTC
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
       25.           └─vctrs:::stop_vctrs(...)
       26.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 3 | WARN 0 | SKIP 99 | PASS 152 ]
      Error: Test failures
      In addition: There were 40 warnings (use warnings() to see them)
      Execution halted
      Warning messages:
      1: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      2: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
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
    
    SUMMARY: processing the following file failed:
      ‘a06_using_cdm_attributes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'CirceR', 'Capr'
    ```

# DrugUtilisation

<details>

* Version: 0.3.3
* GitHub: NA
* Source code: https://github.com/cran/DrugUtilisation
* Date/Publication: 2023-09-25 21:40:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "DrugUtilisation")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DrugUtilisation)
      > 
      > dbToTest <- c(
      +   "duckdb"
      +   #,"sqlserver"
      +   #,"redshift"
    ...
       23.         └─vctrs:::stop_incompatible(...)
       24.           └─vctrs:::stop_vctrs(...)
       25.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 11 | WARN 0 | SKIP 14 | PASS 0 ]
      Error: Test failures
      Execution halted
      Warning messages:
      1: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      2: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
    ```

# editbl

<details>

* Version: 0.9.6
* GitHub: https://github.com/openanalytics/editbl
* Source code: https://github.com/cran/editbl
* Date/Publication: 2023-09-19 11:00:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "editbl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘editbl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rows_delete.tbl_dbi
    > ### Title: rows_delete implementation for DBI backends.
    > ### Aliases: rows_delete.tbl_dbi
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > y <- data.frame(ArtistId = 1)
    > rows_delete(
    +      x = artists,
    +      y = y,
    +      by = "ArtistId",
    +      in_place = TRUE)
    Error in initialize(value, ...) : 
      cannot use object of class “dbplyr_table_ident” in new():  class “SQL” does not extend that class
    Calls: rows_delete ... get_db_table_name -> <Anonymous> -> new -> initialize -> initialize
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(editbl)
      Registered S3 method overwritten by 'editbl':
        method                 from 
        rows_update.data.frame dplyr
      > 
      > test_check("editbl")
    ...
       15.   ├─.transformer(expr, env) %||% .null
       16.   └─glue (local) .transformer(expr, env)
       17.     └─base::eval(parse(text = text, keep.source = FALSE), envir)
       18.       └─base::eval(parse(text = text, keep.source = FALSE), envir)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 53 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# IncidencePrevalence

<details>

* Version: 0.4.1
* GitHub: NA
* Source code: https://github.com/cran/IncidencePrevalence
* Date/Publication: 2023-07-11 12:00:31 UTC
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
      [ FAIL 16 | WARN 0 | SKIP 75 | PASS 81 ]
      
      ══ Skipped tests (75) ══════════════════════════════════════════════════════════
    ...
      Execution halted
      Warning messages:
      1: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      2: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      3: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      4: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      5: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      6: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      7: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      8: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’ using rmarkdown
    
    Quitting from lines 54-58 [unnamed-chunk-4] (a01_Introduction_to_IncidencePrevalence.Rmd)
    Error: processing vignette 'a01_Introduction_to_IncidencePrevalence.Rmd' failed with diagnostics:
    Can't convert `x` <dbplyr_table_ident> to <character>.
    --- failed re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’
    
    --- re-building ‘a02_Creating_denominator_populations.Rmd’ using rmarkdown
    
    ...
      ‘a04_Calculating_prevalence.Rmd’ ‘a05_Calculating_incidence.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ```

# mlr3db

<details>

* Version: 0.5.0
* GitHub: https://github.com/mlr-org/mlr3db
* Source code: https://github.com/cran/mlr3db
* Date/Publication: 2022-08-08 10:10:02 UTC
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
      [ FAIL 9 | WARN 1 | SKIP 0 | PASS 1021 ]
    ...
       25.         │ └─base::lapply(...)
       26.         ├─private$.data$ops
       27.         └─dbplyr:::`$.tbl_lazy`(private$.data, "ops")
       28.           └─cli::cli_abort(...)
       29.             └─rlang::abort(...)
      
      [ FAIL 9 | WARN 1 | SKIP 0 | PASS 1021 ]
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

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/PatientProfiles
* Date/Publication: 2023-08-25 20:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "PatientProfiles")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(PatientProfiles)
      > 
      > availableConnections <- list(list(
      +   con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
      +   write_schema = "main"
    ...
      3: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      4: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      5: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      Execution halted
      Warning messages:
      1: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      2: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      3: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      4: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      5: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
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
    
    --- re-building ‘addPatientCharacteristics.rmd’ using rmarkdown
    ...
    
    SUMMARY: processing the following files failed:
      ‘addCohortIntersections.Rmd’ ‘addPatientCharacteristics.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
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

