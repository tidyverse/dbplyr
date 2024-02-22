# Andromeda

<details>

* Version: 0.6.5
* GitHub: https://github.com/OHDSI/Andromeda
* Source code: https://github.com/cran/Andromeda
* Date/Publication: 2024-01-22 15:32:47 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "Andromeda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Andromeda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: groupApply
    > ### Title: Apply a function to groups of data in an Andromeda table
    > ### Aliases: groupApply
    > 
    > ### ** Examples
    > 
    > andr <- andromeda(cars = cars)
    ...
     12. │           │   └─rlang::is_character(x)
     13. │           └─dbplyr::translate_sql_(op$order_by, con = con)
     14. │             └─base::lapply(...)
     15. │               └─dbplyr (local) FUN(X[[i]], ...)
     16. │                 ├─dbplyr::escape(eval_tidy(x, mask), con = con)
     17. │                 └─rlang::eval_tidy(x, mask)
     18. └─rlang::sym
     19.   └─cli::cli_abort(...)
     20.     └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("Andromeda")
      Loading required package: Andromeda
      Loading required package: dplyr
      
      Attaching package: 'dplyr'
      
    ...
       21. │               └─dbplyr (local) FUN(X[[i]], ...)
       22. │                 ├─dbplyr::escape(eval_tidy(x, mask), con = con)
       23. │                 └─rlang::eval_tidy(x, mask)
       24. └─rlang::sym
       25.   └─cli::cli_abort(...)
       26.     └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 95 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘UsingAndromeda.Rmd’ using rmarkdown
    
    Quitting from lines 175-184 [unnamed-chunk-17] (UsingAndromeda.Rmd)
    Error: processing vignette 'UsingAndromeda.Rmd' failed with diagnostics:
    No known SQL translation
    --- failed re-building ‘UsingAndromeda.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘UsingAndromeda.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘UsingAndromeda.Rmd’ using rmarkdown
    
    tlmgr: Remote database (revision 70006 of the texlive-scripts package)
    seems to be older than the local installation (rev 70039 of
    texlive-scripts); please use a different mirror and/or wait a day or two.
    
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/bookmark.sty'' had status 1
    ...
    
    Error: processing vignette 'UsingAndromeda.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/Andromeda/old/Andromeda.Rcheck/vign_test/Andromeda/vignettes/UsingAndromeda.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See UsingAndromeda.log for more info.
    --- failed re-building ‘UsingAndromeda.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘UsingAndromeda.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bcdata

<details>

* Version: 0.4.1
* GitHub: https://github.com/bcgov/bcdata
* Source code: https://github.com/cran/bcdata
* Date/Publication: 2023-03-18 14:50:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "bcdata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Copyright 2019 Province of British Columbia
      > #
      > # Licensed under the Apache License, Version 2.0 (the "License");
      > # you may not use this file except in compliance with the License.
      > # You may obtain a copy of the License at
      > #
      > # http://www.apache.org/licenses/LICENSE-2.0
    ...
       14.                   └─base::lapply(call[-1], partial_eval, data = data, env = env)
       15.                     └─dbplyr (local) FUN(X[[i]], ...)
       16.                       └─dbplyr:::partial_eval_sym(call, data, env)
       17.                         └─dbplyr:::error_embed("a data.frame", paste0(name, "$x"))
       18.                           └─cli::cli_abort(...)
       19.                             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 105 | PASS 77 ]
      Error: Test failures
      Execution halted
    ```

# CDMConnector

<details>

* Version: 1.3.0
* GitHub: https://github.com/darwin-eu/CDMConnector
* Source code: https://github.com/cran/CDMConnector
* Date/Publication: 2024-02-05 14:20:13 UTC
* Number of recursive dependencies: 147

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
      In addition: Warning messages:
      1: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      2: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      3: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      4: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      5: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      6: Connection is garbage-collected, use dbDisconnect() to avoid this. 
      7: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
      8: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this. 
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
    --- finished re-building ‘a01_getting-started.Rmd’
    
    ...
    No known SQL translation
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
      'arrow', 'CirceR', 'Capr'
    ```

# childesr

<details>

* Version: 0.2.3
* GitHub: https://github.com/langcog/childesr
* Source code: https://github.com/cran/childesr
* Date/Publication: 2022-01-27 00:00:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "childesr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘access_childes_db.Rmd’ using rmarkdown
    
    Quitting from lines 101-103 [unnamed-chunk-8] (access_childes_db.Rmd)
    Error: processing vignette 'access_childes_db.Rmd' failed with diagnostics:
    Failed to collect lazy table.
    Caused by error in `dbSendQuery()`:
    ! could not run statement: Unknown column 'participant.*' in 'field list'
    --- failed re-building ‘access_childes_db.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘access_childes_db.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# CohortSurvival

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/CohortSurvival
* Date/Publication: 2024-02-08 09:50:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "CohortSurvival")` for more info

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
       30. │                     └─dbplyr (local) FUN(X[[i]], ...)
       31. │                       ├─dbplyr::escape(eval_tidy(x, mask), con = con)
       32. │                       └─rlang::eval_tidy(x, mask)
       33. └─CDMConnector::datediff
       34.   └─cli::cli_abort(...)
       35.     └─rlang::abort(...)
      
      [ FAIL 2 | WARN 4 | SKIP 42 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘a01_Single_event_of_interest.Rmd’ using rmarkdown
    
    Quitting from lines 47-53 [unnamed-chunk-5] (a01_Single_event_of_interest.Rmd)
    Error: processing vignette 'a01_Single_event_of_interest.Rmd' failed with diagnostics:
    No known SQL translation
    --- failed re-building ‘a01_Single_event_of_interest.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    ...
    No known SQL translation
    --- failed re-building ‘a02_Competing_risk_survival.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘a01_Single_event_of_interest.Rmd’ ‘a02_Competing_risk_survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# diseasystore

<details>

* Version: 0.1.1
* GitHub: https://github.com/ssi-dk/diseasystore
* Source code: https://github.com/cran/diseasystore
* Date/Publication: 2024-01-18 13:40:02 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "diseasystore")` for more info

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
       33. │                     │ └─methods::initialize(value, ...)
       34. │                     └─RSQLite:::result_create(conn@ptr, statement)
       35. └─base::.handleSimpleError(...)
       36.   └─dbplyr (local) h(simpleError(msg, call))
       37.     └─cli::cli_abort(msg, parent = cnd, call = call, .envir = env)
       38.       └─rlang::abort(...)
      
      [ FAIL 4 | WARN 1 | SKIP 6 | PASS 310 ]
      Error: Test failures
      Execution halted
    ```

# DrugUtilisation

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/DrugUtilisation
* Date/Publication: 2024-02-10 18:10:02 UTC
* Number of recursive dependencies: 163

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
      Backtrace:
          ▆
       1. ├─testthat::expect_true(inherits(x, "omop_result")) at test-summariseTreatment.R:11:3
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─base::inherits(x, "omop_result")
      
      [ FAIL 13 | WARN 9 | SKIP 19 | PASS 115 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_introCreateCohort.Rmd’ using rmarkdown
    
    Quitting from lines 116-122 [unnamed-chunk-6] (a01_introCreateCohort.Rmd)
    Error: processing vignette 'a01_introCreateCohort.Rmd' failed with diagnostics:
    No known SQL translation
    --- failed re-building ‘a01_introCreateCohort.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ...
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘a01_introCreateCohort.Rmd’ ‘a03_addIndications-example.Rmd’
      ‘a04_addDrugInfo.Rmd’ ‘a06_treatmentSummary.Rmd’
      ‘routePatternDose.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# IncidencePrevalence

<details>

* Version: 0.7.0
* GitHub: NA
* Source code: https://github.com/cran/IncidencePrevalence
* Date/Publication: 2024-02-15 22:50:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "IncidencePrevalence")` for more info

</details>

## Newly broken

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
      [ FAIL 18 | WARN 0 | SKIP 72 | PASS 122 ]
      
    ...
       24. │                       └─dbplyr (local) FUN(X[[i]], ...)
       25. │                         ├─dbplyr::escape(eval_tidy(x, mask), con = con)
       26. │                         └─rlang::eval_tidy(x, mask)
       27. └─CDMConnector::dateadd
       28.   └─cli::cli_abort(...)
       29.     └─rlang::abort(...)
      
      [ FAIL 18 | WARN 0 | SKIP 72 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’ using rmarkdown
    
    Quitting from lines 73-81 [unnamed-chunk-6] (a01_Introduction_to_IncidencePrevalence.Rmd)
    Error: processing vignette 'a01_Introduction_to_IncidencePrevalence.Rmd' failed with diagnostics:
    No known SQL translation
    --- failed re-building ‘a01_Introduction_to_IncidencePrevalence.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    ...
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘a01_Introduction_to_IncidencePrevalence.Rmd’
      ‘a02_Creating_denominator_populations.Rmd’
      ‘a03_Creating_target_denominator_populations.Rmd’
      ‘a04_Calculating_prevalence.Rmd’ ‘a05_Calculating_incidence.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PatientProfiles

<details>

* Version: 0.6.1
* GitHub: NA
* Source code: https://github.com/cran/PatientProfiles
* Date/Publication: 2024-02-21 23:30:07 UTC
* Number of recursive dependencies: 174

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
       26. │                       └─dbplyr (local) FUN(X[[i]], ...)
       27. │                         ├─dbplyr::escape(eval_tidy(x, mask), con = con)
       28. │                         └─rlang::eval_tidy(x, mask)
       29. └─CDMConnector::datediff
       30.   └─cli::cli_abort(...)
       31.     └─rlang::abort(...)
      
      [ FAIL 65 | WARN 0 | SKIP 1 | PASS 201 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘addCohortIntersections.Rmd’ using rmarkdown
    
    Quitting from lines 71-81 [unnamed-chunk-4] (addCohortIntersections.Rmd)
    Error: processing vignette 'addCohortIntersections.Rmd' failed with diagnostics:
    No known SQL translation
    --- failed re-building ‘addCohortIntersections.Rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    ...
    No known SQL translation
    --- failed re-building ‘addPatientCharacteristics.rmd’
    
    Warning: Connection is garbage-collected, use dbDisconnect() to avoid this.
    Warning: Database is garbage-collected, use dbDisconnect(con, shutdown=TRUE) or duckdb::duckdb_shutdown(drv) to avoid this.
    SUMMARY: processing the following files failed:
      ‘addCohortIntersections.Rmd’ ‘addPatientCharacteristics.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pool

<details>

* Version: 1.0.3
* GitHub: https://github.com/rstudio/pool
* Source code: https://github.com/cran/pool
* Date/Publication: 2024-02-14 17:30:08 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "pool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pool-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tbl.Pool
    > ### Title: Use pool with dbplyr
    > ### Aliases: tbl.Pool copy_to.Pool
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > 
    > pool <- dbPool(RSQLite::SQLite())
    > # copy a table into the database
    > copy_to(pool, mtcars, "mtcars", temporary = FALSE)
    Error in UseMethod("sql_escape_ident") : 
      no applicable method for 'sql_escape_ident' applied to an object of class "c('Pool', 'R6')"
    Calls: copy_to ... as_table_path -> make_table_path -> sql_escape_ident
    Execution halted
    <pool> Checked-out object deleted before being returned.
    <pool> Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`
    ```

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
       3.   └─pool:::tbl.Pool(dest, name)
       4.     └─dbplyr::tbl_sql("Pool", dbplyr::src_dbi(src), from, ..., vars = vars)
       5.       └─dbplyr:::as_table_source(from, con = src$con)
       6.         └─dbplyr::as_table_path(x, con = con, error_arg = error_arg, error_call = error_call)
       7.           └─dbplyr:::make_table_path(list(x$schema, x$table), con)
       8.             └─dbplyr:::sql_escape_ident(con, x[needs_quote])
      
      [ FAIL 5 | WARN 0 | SKIP 19 | PASS 40 ]
      Error: Test failures
      Execution halted
    ```

# SCDB

<details>

* Version: 0.3
* GitHub: https://github.com/ssi-dk/SCDB
* Source code: https://github.com/cran/SCDB
* Date/Publication: 2024-01-13 01:10:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "SCDB")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SCDB-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_schema
    > ### Title: Get the current schema of a database-related objects
    > ### Aliases: get_schema
    > 
    > ### ** Examples
    > 
    > conn <- get_connection(drv = RSQLite::SQLite())
    ...
    > 
    > dplyr::copy_to(conn, mtcars, name = "mtcars")
    > 
    > get_schema(conn)
    [1] "temp"
    > get_schema(get_table(conn, id("mtcars", conn = conn)))
    Error in unclass(dbplyr::remote_table(.x))$schema : 
      $ operator is invalid for atomic vectors
    Calls: get_schema -> get_schema.tbl_dbi
    Execution halted
    ```

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
          temporary = temporary, unique_indexes = unique_indexes, indexes = indexes, 
          analyze = analyze, in_transaction = in_transaction, ...)`: Can't copy data to table `SCDB_log_patch`.
      Caused by error in `dplyr::db_write_table()`:
      ! Can't write table table `SCDB_log_patch`.
      Caused by error:
      ! Table `SCDB_log_patch` exists in database, and both overwrite and append are FALSE
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 294 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic-principles.Rmd’ using rmarkdown
    
    Quitting from lines 76-82 [example_1] (basic-principles.Rmd)
    Error: processing vignette 'basic-principles.Rmd' failed with diagnostics:
    error in evaluating the argument 'name' in selecting a method for function 'dbRemoveTable': invalid 'envir' argument of type 'character'
    --- failed re-building ‘basic-principles.Rmd’
    
    Error in `purrr::map_chr()`:
    ℹ In index: 2.
    ...
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    --- re-building ‘slowly-changing-dimension.Rmd’ using rmarkdown
    --- finished re-building ‘slowly-changing-dimension.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-principles.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

