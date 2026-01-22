# Andromeda (1.2.0)

* GitHub: <https://github.com/OHDSI/Andromeda>
* Email: <mailto:schuemie@ohdsi.org>
* GitHub mirror: <https://github.com/cran/Andromeda>

Run `revdepcheck::cloud_details(, "Andromeda")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘Andromeda-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: appendToTable
     > ### Title: Append to an Andromeda table
     > ### Aliases: appendToTable
     > 
     > ### ** Examples
     > 
     > andr <- andromeda(cars = cars)
     > nrow(andr$cars)
     [1] NA
     > # [1] 50
     > 
     > appendToTable(andr$cars, cars)
     Error in `appendToTable()`:
     ! First argument must be an Andromeda table
     Backtrace:
         ▆
      1. └─Andromeda::appendToTable(andr$cars, cars)
      2.   └─rlang::abort("First argument must be an Andromeda table")
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Backtrace:
           ▆
        1. ├─methods (local) `$<-`(`*tmp*`, CARS, value = `<tbl_Andr[,2]>`) at test-object.R:183:3
        2. └─Andromeda (local) `$<-`(`*tmp*`, CARS, value = `<tbl_Andr[,2]>`)
        3.   ├─methods (local) `[[<-`(`*tmp*`, name, value = `<tbl_Andr[,2]>`)
        4.   └─Andromeda (local) `[[<-`(`*tmp*`, name, value = `<tbl_Andr[,2]>`)
        5.     └─Andromeda (local) .local(x, i, ..., value)
        6.       └─rlang::abort("Table must be a data frame or dplyr table")
       ── Failure ('test-object.R:192:3'): isAndromedaTable sqlite version ────────────
       Expected `isAndromedaTable(a$cars)` to be TRUE.
       Differences:
       `actual`:   FALSE
       `expected`: TRUE 
       
       ── Failure ('test-object.R:193:3'): isAndromedaTable sqlite version ────────────
       Expected `isAndromedaTable(dplyr::mutate(a$cars, a = 1))` to be TRUE.
       Differences:
       `actual`:   FALSE
       `expected`: TRUE 
       
       
       [ FAIL 22 | WARN 0 | SKIP 1 | PASS 60 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘UsingAndromeda.Rmd’ using rmarkdown
     
     Quitting from UsingAndromeda.Rmd:54-56 [unnamed-chunk-5]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `appendToTable()`:
     ! First argument must be an Andromeda table
     ---
     Backtrace:
         ▆
      1. └─Andromeda::appendToTable(andr$cars, cars)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'UsingAndromeda.Rmd' failed with diagnostics:
     First argument must be an Andromeda table
     --- failed re-building ‘UsingAndromeda.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘UsingAndromeda.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# arkdb (0.0.18)

* GitHub: <https://github.com/ropensci/arkdb>
* Email: <mailto:cboettig@gmail.com>
* GitHub mirror: <https://github.com/cran/arkdb>

Run `revdepcheck::cloud_details(, "arkdb")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Saving _problems/test-arkdb-121.R
       [ FAIL 3 | WARN 7 | SKIP 8 | PASS 51 ]
       
       ══ Skipped tests (8) ═══════════════════════════════════════════════════════════
       • On CRAN (6): 'test-arkdb.R:287:3', 'test-bulk-import.R:6:3',
         'test-bulk-import.R:72:3', 'test-errors.R:12:3', 'test-errors.R:34:3',
         'test-errors.R:53:3'
       • {MonetDBLite} is not installed (2): 'test-arkdb.R:132:3',
         'test-local_db.R:24:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-arkdb.R:42:3'): we can ark and unark a db ────────────────────
       Expected `myflights` to inherit from "character".
       Actual inheritance: "tbl_duckdb_connection"/"tbl_sql"/"tbl_lazy"/"tbl"
       ── Failure ('test-arkdb.R:80:3'): we can ark and unark a db in plain text ──────
       Expected `myflights` to inherit from "character".
       Actual inheritance: "tbl_duckdb_connection"/"tbl_sql"/"tbl_lazy"/"tbl"
       ── Failure ('test-arkdb.R:121:3'): alternate method for ark ────────────────────
       Expected `myflights` to inherit from "character".
       Actual inheritance: "tbl_duckdb_connection"/"tbl_sql"/"tbl_lazy"/"tbl"
       
       [ FAIL 3 | WARN 7 | SKIP 8 | PASS 51 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# bigrquery (1.6.1)

* GitHub: <https://github.com/r-dbi/bigrquery>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/bigrquery>

Run `revdepcheck::cloud_details(, "bigrquery")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       `expected`: "SAFE_CAST(`x` AS INT64)"             
       
       ── Failure ('test-dplyr.R:153:3'): casting uses bigquery types ─────────────────
       Expected `sql$select[[3]]` to equal "SAFE_CAST(`x` AS FLOAT64)".
       Differences:
       `class(actual)` is a character vector ('sql', 'character')
       `class(expected)` is absent
       
       `actual`:   "<SQL> SAFE_CAST(`x` AS FLOAT64) AS `z`"
       `expected`: "SAFE_CAST(`x` AS FLOAT64)"             
       
       ── Failure ('test-dplyr.R:163:3'): %||% translates to IFNULL ───────────────────
       Expected `sql$select[[2]]` to equal "IFNULL(`x`, 2)".
       Differences:
       `class(actual)` is a character vector ('sql', 'character')
       `class(expected)` is absent
       
       `actual`:   "<SQL> IFNULL(`x`, 2) AS `y`"
       `expected`: "IFNULL(`x`, 2)"             
       
       
       [ FAIL 3 | WARN 0 | SKIP 97 | PASS 119 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# CDMConnector (2.3.0)

* GitHub: <https://github.com/darwin-eu/CDMConnector>
* Email: <mailto:black@ohdsi.org>
* GitHub mirror: <https://github.com/cran/CDMConnector>

Run `revdepcheck::cloud_details(, "CDMConnector")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        14. │ └─dbplyr:::collect.tbl_sql(x)
        15. │   └─dbplyr::db_sql_render(x$con, x, cte = cte)
        16. │     ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        17. │     └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        18. │       ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        19. │       └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        20. │         ├─dbplyr::sql_render(...)
        21. │         └─dbplyr:::sql_render.lazy_query(...)
        22. │           ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        23. │           └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        24. │             └─dbplyr:::get_select_sql(...)
        25. │               └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        26. │                 └─base::lapply(...)
        27. │                   └─dbplyr (local) FUN(X[[i]], ...)
        28. │                     ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        29. │                     └─rlang::eval_tidy(x, mask)
        30. └─dplyr::n_distinct(subject_id)
        31.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        32.     └─utils::getFromNamespace(fun, pkg)
        33.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 7 | WARN 0 | SKIP 44 | PASS 53 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      15. │               ├─dbplyr::sql_render(...)
      16. │               └─dbplyr:::sql_render.lazy_query(...)
      17. │                 ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
      18. │                 └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
      19. │                   └─dbplyr:::get_select_sql(...)
      20. │                     └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
      21. │                       └─base::lapply(...)
      22. │                         └─dbplyr (local) FUN(X[[i]], ...)
      23. │                           ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      24. │                           └─rlang::eval_tidy(x, mask)
      25. └─dplyr::n_distinct(subject_id)
      26.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      27.     └─utils::getFromNamespace(fun, pkg)
      28.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'a06_using_cdm_attributes.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘a06_using_cdm_attributes.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘a06_using_cdm_attributes.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# ckanr (0.7.0)

* GitHub: <https://github.com/ropensci/ckanr>
* Email: <mailto:fjunior.alves.oliveira@gmail.com>
* GitHub mirror: <https://github.com/cran/ckanr>

Run `revdepcheck::cloud_details(, "ckanr")` for more info

## Newly broken

*   checking whether package ‘ckanr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/ckanr/new/ckanr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘ckanr’ ...
** this is package ‘ckanr’ version ‘0.7.0’
** package ‘ckanr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘src_sql’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘ckanr’
* removing ‘/tmp/workdir/ckanr/new/ckanr.Rcheck/ckanr’


```
### CRAN

```
* installing *source* package ‘ckanr’ ...
** this is package ‘ckanr’ version ‘0.7.0’
** package ‘ckanr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ckanr)


```
# CohortCharacteristics (1.1.0)

* GitHub: <https://github.com/darwin-eu/CohortCharacteristics>
* Email: <mailto:marti.catalasabate@ndorms.ox.ac.uk>
* GitHub mirror: <https://github.com/cran/CohortCharacteristics>

Run `revdepcheck::cloud_details(, "CohortCharacteristics")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        10. │         └─dbplyr:::collect.tbl_sql(x)
        11. │           └─dbplyr::db_sql_render(x$con, x, cte = cte)
        12. │             ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        13. │             └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        14. │               ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        15. │               └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        16. │                 ├─dbplyr::sql_render(...)
        17. │                 └─dbplyr:::sql_render.lazy_query(...)
        18. │                   ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        19. │                   └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        20. │                     └─dbplyr:::get_select_sql(...)
        21. │                       └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        22. │                         └─base::lapply(...)
        23. │                           └─dbplyr (local) FUN(X[[i]], ...)
        24. │                             ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        25. │                             └─rlang::eval_tidy(x, mask)
        26. └─dplyr::n_distinct(subject_id)
        27.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        28.     └─utils::getFromNamespace(fun, pkg)
        29.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 8 | WARN 16 | SKIP 24 | PASS 108 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      22. │           ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
      23. │           └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
      24. │             └─dbplyr:::get_select_sql(...)
      25. │               └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
      26. │                 └─base::lapply(...)
      27. │                   └─dbplyr (local) FUN(X[[i]], ...)
      28. │                     ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      29. │                     └─rlang::eval_tidy(x, mask)
      30. └─dplyr::n_distinct(subject_id)
      31.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      32.     └─utils::getFromNamespace(fun, pkg)
      33.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'summarise_large_scale_characteristics.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘summarise_large_scale_characteristics.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘summarise_characteristics.Rmd’ ‘summarise_cohort_overlap.Rmd’
       ‘summarise_cohort_timing.Rmd’
       ‘summarise_large_scale_characteristics.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# CohortSymmetry (0.2.4)

* GitHub: <https://github.com/OHDSI/CohortSymmetry>
* Email: <mailto:xihang.chen@ndorms.ox.ac.uk>
* GitHub mirror: <https://github.com/cran/CohortSymmetry>

Run `revdepcheck::cloud_details(, "CohortSymmetry")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         7. │     └─dbplyr:::collect.tbl_sql(x)
         8. │       └─dbplyr::db_sql_render(x$con, x, cte = cte)
         9. │         ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        10. │         └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        11. │           ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        12. │           └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        13. │             ├─dbplyr::sql_render(...)
        14. │             └─dbplyr:::sql_render.lazy_query(...)
        15. │               ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        16. │               └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        17. │                 └─dbplyr:::get_select_sql(...)
        18. │                   └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        19. │                     └─base::lapply(...)
        20. │                       └─dbplyr (local) FUN(X[[i]], ...)
        21. │                         ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        22. │                         └─rlang::eval_tidy(x, mask)
        23. └─dplyr::n_distinct(cohort_definition_id)
        24.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        25.     └─utils::getFromNamespace(fun, pkg)
        26.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 9 | WARN 36 | SKIP 55 | PASS 0 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      22. │                         └─dbplyr:::get_select_sql(...)
      23. │                           └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
      24. │                             └─base::lapply(...)
      25. │                               └─dbplyr (local) FUN(X[[i]], ...)
      26. │                                 ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      27. │                                 └─rlang::eval_tidy(x, mask)
      28. └─dplyr::n_distinct(subject_id)
      29.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      30.     └─utils::getFromNamespace(fun, pkg)
      31.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'a06_Visualise_temporal_symmetry.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘a06_Visualise_temporal_symmetry.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘a01_Introduction.Rmd’ ‘a02_Generate_a_sequence_cohort.Rmd’
       ‘a03_Summarise_sequence_ratios.Rmd’
       ‘a04_Visualise_sequence_ratios.Rmd’
       ‘a05_Summarise_temporal_symmetry.Rmd’
       ‘a06_Visualise_temporal_symmetry.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# connector (1.0.0)

* GitHub: <https://github.com/NovoNordisk-OpenSource/connector>
* Email: <mailto:cgid@novonordisk.com>
* GitHub mirror: <https://github.com/cran/connector>

Run `revdepcheck::cloud_details(, "connector")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Saving _problems/test-connect-32.R
       [ FAIL 1 | WARN 2 | SKIP 26 | PASS 191 ]
       
       ══ Skipped tests (26) ══════════════════════════════════════════════════════════
       • On CRAN (25): 'test-cnt_logger_integration.R:2:3', 'test-connect.R:1:1',
         'test-connector.R:1:1', 'test-connector.R:31:1', 'test-connector.R:31:1',
         'test-connector.R:31:1', 'test-connector.R:31:1', 'test-connector.R:42:1',
         'test-connector.R:42:1', 'test-connector.R:42:1', 'test-connector.R:42:1',
         'test-metadata_connectors.R:85:1', 'test-metadata_connectors.R:85:1',
         'test-metadata_connectors.R:85:1', 'test-metadata_connectors.R:85:1',
         'test-use_template.R:2:3', 'test-utils_files.R:1:1',
         'test-utils_files.R:1:1', 'test-utils_files.R:1:1', 'test-utils_files.R:1:1',
         'test-utils_files.R:5:1', 'test-utils_files.R:5:1', 'test-utils_files.R:5:1',
         'test-utils_files.R:5:1', 'test-utils_files.R:9:1'
       • postgres database not available (1): 'test-dbi.R:34:9'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-connect.R:32:3'): Connect datasources to the connections for a yaml file ──
       Expected `iris_f` to inherit from "tbl_dbi".
       Actual class: "tbl_SQLiteConnection"/"tbl_sql"/"tbl_lazy"/"tbl".
       
       [ FAIL 1 | WARN 2 | SKIP 26 | PASS 191 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# DatabaseConnector (7.1.0)

* GitHub: <https://github.com/OHDSI/DatabaseConnector>
* Email: <mailto:schuemie@ohdsi.org>
* GitHub mirror: <https://github.com/cran/DatabaseConnector>

Run `revdepcheck::cloud_details(, "DatabaseConnector")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'reexports.Rd':
       ‘[dbplyr:db-quote]{sql_escape_logical}’
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

# dbi.table (1.0.6)

* GitHub: <https://github.com/kjellpk/dbi.table>
* Email: <mailto:kjellk@gmail.com>
* GitHub mirror: <https://github.com/cran/dbi.table>

Run `revdepcheck::cloud_details(, "dbi.table")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > # It is recommended that you do not modify it.
       > #
       > # Where should you do additional test configuration?
       > # Learn more about the roles of various files in:
       > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
       > # * https://testthat.r-lib.org/articles/special-files.html
       > 
       > library(testthat)
       > library(dbi.table)
       > 
       > test_check("dbi.table")
       Saving _problems/test-assumptions-10.R
       [ FAIL 1 | WARN 0 | SKIP 2 | PASS 417 ]
       
       ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
       • On CRAN (2): 'test-mariadb.R:10:3', 'test-postgres.R:11:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-assumptions.R:8:3'): dbplyr::translate_sql_ borks ────────────
       Expected `{ ... }` to throw a error.
       
       [ FAIL 1 | WARN 0 | SKIP 2 | PASS 417 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# diseasystore (0.3.2)

* GitHub: <https://github.com/ssi-dk/diseasystore>
* Email: <mailto:rske@ssi.dk>
* GitHub mirror: <https://github.com/cran/diseasystore>

Run `revdepcheck::cloud_details(, "diseasystore")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        11.   └─purrr (local) h(simpleError(msg, call))
        12.     └─cli::cli_abort(...)
        13.       └─rlang::abort(...)
       ── Error ('test-truncate_interlace.R:23:5'): truncate_interlace works ──────────
       Error in `UseMethod("id")`: no applicable method for 'id' applied to an object of class "c('tbl_SQLiteConnection', 'tbl_sql', 'tbl_lazy', 'tbl')"
       Backtrace:
           ▆
        1. └─SCDB::defer_db_cleanup(x) at test-truncate_interlace.R:23:5
        2.   └─SCDB::id(db_table)
       ── Failure ('test-zzz.R:12:3'): data is not written locally ────────────────────
       Expected `dir(recursive = TRUE)` to have the same values as `current_files`.
       Actual: "_problems/test-DiseasystoreBase-134.R", "_problems/test-DiseasystoreBase-179.R", "_problems/test-DiseasystoreBase-290.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-age_helpers-155.R", "_problems/test-age_helpers-215.R", "_problems/test-age_helpers-276.R", "_problems/test-age_helpers-341.R", ...
       Expected: "helper-setup.R", "setup.R", "test-0_R6_utils.R", "test-0_documentation.R", "test-0_linters.R", "test-0_rd_files.R", "test-DiseasystoreBase.R", "test-DiseasystoreEcdcRespitoryViruses.R", "test-DiseasystoreGoogleCovid19.R", ...
       Needs: "_problems/test-DiseasystoreBase-134.R", "_problems/test-DiseasystoreBase-179.R", "_problems/test-DiseasystoreBase-290.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-age_helpers-155.R", "_problems/test-age_helpers-215.R", "_problems/test-age_helpers-276.R", "_problems/test-age_helpers-341.R", ...
       
       [ FAIL 31 | WARN 4 | SKIP 12 | PASS 385 ]
       Error:
       ! Test failures.
       Warning messages:
       1: In connection_release(conn@ptr) : Already disconnected
       2: call dbDisconnect() when finished working with a connection 
       Execution halted
       Warning messages:
       1: In connection_release(conn@ptr) : Already disconnected
       2: In connection_release(conn@ptr) : Already disconnected
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     
     Quitting from extending-diseasystore-example.Rmd:472-478 [get_feature_sex]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error:
     ! Lock not released within 30 minutes. Giving up.
     ---
     Backtrace:
         ▆
      1. └─ds$get_feature(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'extending-diseasystore-example.Rmd' failed with diagnostics:
     Lock not released within 30 minutes. Giving up.
     --- failed re-building ‘extending-diseasystore-example.Rmd’
     
     Warning: Connection already closed.
     --- re-building ‘extending-diseasystore.Rmd’ using rmarkdown
     --- finished re-building ‘extending-diseasystore.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘extending-diseasystore-example.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# dm (1.0.12)

* GitHub: <https://github.com/cynkra/dm>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/dm>

Run `revdepcheck::cloud_details(, "dm")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       4. │   └─base::eval(ei, envir)
       5. │     └─base::eval(ei, envir)
       6. ├─dm::dm_sql(dm, con)
       7. │ └─dm::dm_ddl_post(dm, dest, table_names, temporary)
       8. │   └─dm:::ddl_get_index_defs(fks, con, table_names)
       9. │     └─... %>% ...
      10. ├─dplyr::summarize(...)
      11. ├─dplyr::group_by(., name)
      12. ├─dplyr::mutate(...)
      13. ├─dplyr:::mutate.data.frame(...)
      14. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      15. │   ├─base::withCallingHandlers(...)
      16. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      17. │     └─mask$eval_all_mutate(quo)
      18. │       └─dplyr (local) eval()
      19. ├─purrr::map_chr(...)
      20. │ └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
      21. │   └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
      22. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      23. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      24. │ └─base::stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
      25. └─base::.handleSimpleError(...)
      26.   └─dplyr (local) h(simpleError(msg, call))
      27.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         Ran 7/7 deferred expressions
         Error in deferred_run(env) : could not find function "deferred_run"
         Calls: <Anonymous>
         
       Backtrace:
            ▆
         1. └─testthat::test_check("dm")
         2.   └─testthat::test_dir(...)
         3.     └─testthat:::test_files(...)
         4.       └─testthat:::test_files_parallel(...)
         5.         ├─withr::with_dir(...)
         6.         │ └─base::force(code)
         7.         ├─testthat::with_reporter(...)
         8.         │ └─base::tryCatch(...)
         9.         │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        10.         │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        11.         │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        12.         └─testthat:::parallel_event_loop_chunky(queue, reporters, ".")
        13.           └─queue$poll(Inf)
        14.             └─base::lapply(...)
        15.               └─testthat (local) FUN(X[[i]], ...)
        16.                 └─private$handle_error(msg, i)
        17.                   └─cli::cli_abort(...)
        18.                     └─rlang::abort(...)
       Execution halted
     ```

# duckdb (1.4.3)

* GitHub: <https://github.com/duckdb/duckdb-r>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/duckdb>

Run `revdepcheck::cloud_details(, "duckdb")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         7. │ └─dbplyr:::collect.tbl_sql(.data)
         8. │   └─dbplyr::db_sql_render(x$con, x, cte = cte)
         9. │     ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        10. │     └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        11. │       ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        12. │       └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        13. │         ├─dbplyr::sql_render(...)
        14. │         └─dbplyr:::sql_render.lazy_query(...)
        15. │           ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        16. │           └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        17. │             └─dbplyr:::get_select_sql(...)
        18. │               └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        19. │                 └─base::lapply(...)
        20. │                   └─dbplyr (local) FUN(X[[i]], ...)
        21. │                     ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        22. │                     └─rlang::eval_tidy(x, mask)
        23. └─duckdb (local) n_distinct(x, na.rm = TRUE)
        24.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        25.     └─utils::getFromNamespace(fun, pkg)
        26.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 3 | WARN 0 | SKIP 75 | PASS 793 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# duckplyr (1.1.3)

* GitHub: <https://github.com/tidyverse/duckplyr>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/duckplyr>

Run `revdepcheck::cloud_details(, "duckplyr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-dplyr-lead-lag.R:94:3', 'test-dplyr-lead-lag.R:104:3',
         'test-dplyr-lead-lag.R:113:3', 'test-dplyr-lead-lag.R:125:3',
         'test-dplyr-lead-lag.R:140:3', 'test-dplyr-lead-lag.R:151:3',
         'test-dplyr-lead-lag.R:159:3', 'test-dplyr-lead-lag.R:168:3',
         'test-dplyr-lead-lag.R:174:3'
       • i In argument: `.result = (1 + "") * am`. (1): 'test-dplyr-conditions.R:25:5'
       • {RSQLite} is not installed (2): 'test-dplyr-count-tally.R:93:3',
         'test-dplyr-count-tally.R:105:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-as_tbl.R:7:3'): as_tbl() ─────────────────────────────────────
       Expected `tbl` to inherit from "tbl_dbi".
       Actual class: "tbl_duckdb_connection"/"tbl_sql"/"tbl_lazy"/"tbl".
       
       [ FAIL 1 | WARN 0 | SKIP 600 | PASS 2350 ]
       Error:
       ! Test failures.
       
       🛠: 2284
       🔨: 1199
       🦆: 1085
       add_count, anti_join, anti_join.data.frame, arrange, arrange.data.frame, compute, count, count.data.frame, cross_join, distinct, distinct.data.frame, do, eval, filter, filter.data.frame, full_join, group_by, group_indices, group_keys, group_map, group_modify, group_nest, group_size, group_split, group_trim, head, inner_join, inner_join.data.frame, intersect, left_join, left_join.data.frame, mutate, mutate.data.frame, n_groups, nest_by, nest_join, pull, reframe, relocate, rename, rename_with, right_join, rows_append, rows_delete, rows_insert, rows_patch, rows_update, rows_upsert, rowwise, select, select.data.frame, semi_join, semi_join.data.frame, setdiff, setequal, slice, slice_head, slice_head.data.frame, slice_sample, slice_tail, summarise, summarise.data.frame, symdiff, transmute, ungroup, union_all
       
       00:01:31.964881
       Execution halted
     ```

# editbl (1.3.0)

* GitHub: <https://github.com/openanalytics/editbl>
* Email: <mailto:jasper.schelfhout@openanalytics.eu>
* GitHub mirror: <https://github.com/cran/editbl>

Run `revdepcheck::cloud_details(, "editbl")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +          ArtistId = c(1,2),
     +          Name = c("AC/DC", "The Offspring")
     + )
     > DBI::dbWriteTable(conn, "Artist", artists_df)     
     >  
     > # Insert new row
     > artists <- tbl(conn, "Artist")
     > DBI::dbBegin(conn)
     > e_rows_insert(artists,
     +  data.frame(ArtistId = 999, Name = "testArtist"),
     +  in_place = TRUE)
     Error in `dplyr::rows_insert()`:
     ! `conflict = "error"` isn't supported on database backends.
     ℹ It must be "ignore" instead.
     Backtrace:
         ▆
      1. ├─editbl::e_rows_insert(...)
      2. └─editbl:::e_rows_insert.default(...)
      3.   ├─dplyr::rows_insert(...)
      4.   └─dbplyr:::rows_insert.tbl_lazy(...)
      5.     └─dbplyr:::rows_check_conflict(conflict)
      6.       └─dbplyr:::check_unsupported_arg(conflict, "ignore", call = error_call)
      7.         └─cli::cli_abort(msg, call = call, class = "dbplyr_error_unsupported_arg")
      8.           └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       `actual`:   FALSE
       `expected`: TRUE 
       
       ── Error ('test-tbl_dbi.R:18:7'): e_rows_update.tbl_dbi works ──────────────────
       <rlib_error_dots_nonempty/rlib_error_dots/rlang_error/error/condition>
       Error in `dplyr::rows_update(x = x, y = y, by = by, ..., match = match, unmatched = unmatched, copy = copy, in_place = in_place)`: `...` must be empty.
       ✖ Problematic argument:
       • match = match
       Backtrace:
           ▆
        1. ├─editbl::e_rows_update(...) at test-tbl_dbi.R:18:7
        2. └─editbl:::e_rows_update.default(...)
        3.   ├─dplyr::rows_update(...)
        4.   └─dbplyr:::rows_update.tbl_lazy(...)
        5.     └─rlang::check_dots_empty()
        6.       └─rlang:::action_dots(...)
        7.         ├─base (local) try_dots(...)
        8.         └─rlang (local) action(...)
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 63 ]
       Error:
       ! Test failures.
       Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
         ▆
      1. └─dm::dm_from_con(conn, learn_keys = FALSE)
      2.   └─dm:::get_src_tbl_names(src, ..., names = .names)
      3.     └─dplyr::src_tbls(src)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'howto_relational_db_dm.rmd' failed with diagnostics:
     no applicable method for 'src_tbls' applied to an object of class "c('src_SQLiteConnection', 'src_dbi', 'src_sql', 'src')"
     --- failed re-building ‘howto_relational_db_dm.rmd’
     
     --- re-building ‘howto_row_level_access.rmd’ using rmarkdown
     Warning: Shiny application in a static R Markdown document
     Warning: Shiny application in a static R Markdown document
     --- finished re-building ‘howto_row_level_access.rmd’
     
     --- re-building ‘howto_switch_from_DT.rmd’ using rmarkdown
     Warning: Shiny application in a static R Markdown document
     Warning: Shiny application in a static R Markdown document
     --- finished re-building ‘howto_switch_from_DT.rmd’
     
     SUMMARY: processing the following file failed:
       ‘howto_relational_db_dm.rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# etl (0.4.2)

* GitHub: <https://github.com/beanumber/etl>
* Email: <mailto:ben.baumer@gmail.com>
* GitHub mirror: <https://github.com/cran/etl>

Run `revdepcheck::cloud_details(, "etl")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     2 0 0 GB /tmp/RtmpWtxhYy/load
           Length Class            Mode
     con   1      SQLiteConnection S4  
     disco 0      -none-           NULL
     > 
     > ## Not run: 
     > ##D # connect to a PostgreSQL server
     > ##D if (require(RPostgreSQL)) {
     > ##D   db <- src_postgres("mtcars", user = "postgres", host = "localhost")
     > ##D   cars <- etl("mtcars", db)
     > ##D }
     > ## End(Not run)
     > 
     > # Do it step-by-step
     > cars |>
     +   etl_extract() |>
     +   etl_transform() |>
     +   etl_load()
     Extracting raw data...
     Loading 1 file(s) into the database...
     > src_tbls(cars)
     Error in UseMethod("src_tbls") : 
       no applicable method for 'src_tbls' applied to an object of class "c('etl_mtcars', 'etl', 'src_SQLiteConnection', 'src_dbi', 'src_sql', 'src')"
     Calls: src_tbls
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
       • On CRAN (2): 'test-etl.R:102:3', 'test-etl.R:119:3'
       • empty test (1): 'test-etl.R:51:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-etl.R:32:3'): dplyr works ──────────────────────────────────────
       Error in `UseMethod("src_tbls")`: no applicable method for 'src_tbls' applied to an object of class "c('etl_mtcars', 'etl', 'src_SQLiteConnection', 'src_dbi', 'src_sql', 'src')"
       Backtrace:
           ▆
        1. ├─testthat::expect_gt(length(src_tbls(cars)), 0) at test-etl.R:32:3
        2. │ └─testthat::quasi_label(enquo(object), label)
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─dplyr::src_tbls(cars)
       ── Failure ('test-etl.R:97:3'): etl works ──────────────────────────────────────
       Expected output from `print(cars)` to match regexp "sqlite".
       Actual output:
       ✖ │ dir:  2 files occupying 0 GB
         │ <S4 class 'SQLiteConnection' [package "RSQLite"] with 8 slots>NULL
       
       [ FAIL 2 | WARN 0 | SKIP 3 | PASS 24 ]
       Error:
       ! Test failures.
       Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- re-building ‘extending_etl.Rmd’ using rmarkdown
     
     Quitting from extending_etl.Rmd:55-59 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `UseMethod()`:
     ! no applicable method for 'src_tbls' applied to an object of class "c('etl_ggplot2', 'etl', 'src_SQLiteConnection', 'src_dbi', 'src_sql', 'src')"
     ---
     Backtrace:
         ▆
      1. └─dplyr::src_tbls(ggplots)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'extending_etl.Rmd' failed with diagnostics:
     no applicable method for 'src_tbls' applied to an object of class "c('etl_ggplot2', 'etl', 'src_SQLiteConnection', 'src_dbi', 'src_sql', 'src')"
     --- failed re-building ‘extending_etl.Rmd’
     
     --- re-building ‘using_etl.Rmd’ using rmarkdown
     --- finished re-building ‘using_etl.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘extending_etl.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# healthdb (0.5.0)

* GitHub: <https://github.com/KevinHzq/healthdb>
* Email: <mailto:kevin.hu@bccdc.ca>
* GitHub mirror: <https://github.com/cran/healthdb>

Run `revdepcheck::cloud_details(, "healthdb")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
      26. │             └─base::lapply(...)
      27. │               └─healthdb (local) FUN(X[[i]], ...)
      28. │                 └─data %>% ...
      29. ├─dplyr::filter(...)
      30. ├─dbplyr:::filter.tbl_lazy(...)
      31. │ └─dbplyr:::partial_eval_dots(.data, ..., .named = FALSE)
      32. │   └─dbplyr:::partial_eval_quo(...)
      33. │     ├─base::withCallingHandlers(...)
      34. │     └─dbplyr::partial_eval(get_expr(x), data, get_env(x), error_call = error_call)
      35. │       └─dbplyr:::partial_eval_if(call, data, env, reduce = "|", error_call = error_call)
      36. │         └─dbplyr:::across_setup(...)
      37. │           └─base::lapply(out, partial_eval, data = data, env = env, error_call = error_call)
      38. │             └─dbplyr (local) FUN(X[[i]], ...)
      39. │               └─dbplyr:::partial_eval_call(call, data, env)
      40. │                 └─base::lapply(call[-1], partial_eval, data = data, env = env)
      41. │                   └─dbplyr (local) FUN(X[[i]], ...)
      42. │                     └─dbplyr:::partial_eval_call(call, data, env)
      43. │                       └─rlang::eval_bare(call, env = env)
      44. ├─dbplyr::sql(dbplyr::escape_ansi(x))
      45. │ └─dbplyr:::c_character(...)
      46. └─base::.handleSimpleError(...)
      47.   └─dbplyr (local) h(simpleError(msg, call))
      48.     └─cli::cli_abort(msg, call = error_call, parent = cnd)
      49.       └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        1. └─dbplyr::memdb_frame(df) at test-restrict_dates_sql.R:47:3
        2.   ├─dplyr::copy_to(memdb(), tibble(...), name = .name)
        3.   └─dplyr:::copy_to.DBIConnection(memdb(), tibble(...), name = .name)
        4.     ├─dplyr::copy_to(...)
        5.     └─dbplyr:::copy_to.src_sql(...)
        6.       └─dbplyr::as_table_path(name, dest$con)
        7.         └─cli::cli_abort(...)
        8.           └─rlang::abort(...)
       ── Error ('test-restrict_dates_sql.R:70:3'): count same date works for database ──
       Error in `as_table_path(name, dest$con)`: `name` uses unknown specification for table name
       Backtrace:
           ▆
        1. └─dbplyr::memdb_frame(df) at test-restrict_dates_sql.R:70:3
        2.   ├─dplyr::copy_to(memdb(), tibble(...), name = .name)
        3.   └─dplyr:::copy_to.DBIConnection(memdb(), tibble(...), name = .name)
        4.     ├─dplyr::copy_to(...)
        5.     └─dbplyr:::copy_to.src_sql(...)
        6.       └─dbplyr::as_table_path(name, dest$con)
        7.         └─cli::cli_abort(...)
        8.           └─rlang::abort(...)
       
       [ FAIL 26 | WARN 143 | SKIP 7 | PASS 243 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      15. │       └─dbplyr:::partial_eval_if(call, data, env, reduce = "|", error_call = error_call)
      16. │         └─dbplyr:::across_setup(...)
      17. │           └─base::lapply(out, partial_eval, data = data, env = env, error_call = error_call)
      18. │             └─dbplyr (local) FUN(X[[i]], ...)
      19. │               └─dbplyr:::partial_eval_call(call, data, env)
      20. │                 └─base::lapply(call[-1], partial_eval, data = data, env = env)
      21. │                   └─dbplyr (local) FUN(X[[i]], ...)
      22. │                     └─dbplyr:::partial_eval_call(call, data, env)
      23. │                       └─rlang::eval_bare(call, env = env)
      24. └─dbplyr::sql(dbplyr::escape_ansi(x))
      25.   └─dbplyr:::c_character(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'healthdb.Rmd' failed with diagnostics:
     ℹ In argument: `dplyr::if_any(dplyr::all_of(vars), ~stringr::str_like(.,
       dbplyr::sql(dbplyr::escape_ansi(x))))`
     Caused by error:
     ! 'escape_ansi' is not an exported object from 'namespace:dbplyr'
     --- failed re-building ‘healthdb.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘healthdb.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

*   checking dependencies in R code ... WARNING
     ```
     Missing or unexported object: ‘dbplyr::escape_ansi’
     ```

# implyr (0.5.0)

* GitHub: <https://github.com/ianmcook/implyr>
* Email: <mailto:ianmcook@gmail.com>
* GitHub mirror: <https://github.com/cran/implyr>

Run `revdepcheck::cloud_details(, "implyr")` for more info

## Newly broken

*   checking whether package ‘implyr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/implyr/new/implyr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘implyr’ ...
** this is package ‘implyr’ version ‘0.5.0’
** package ‘implyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘src_sql’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘implyr’
* removing ‘/tmp/workdir/implyr/new/implyr.Rcheck/implyr’


```
### CRAN

```
* installing *source* package ‘implyr’ ...
** this is package ‘implyr’ version ‘0.5.0’
** package ‘implyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (implyr)


```
# IncidencePrevalence (1.2.1)

* GitHub: <https://github.com/darwin-eu/IncidencePrevalence>
* Email: <mailto:edward.burn@ndorms.ox.ac.uk>
* GitHub mirror: <https://github.com/cran/IncidencePrevalence>

Run `revdepcheck::cloud_details(, "IncidencePrevalence")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        11. │           └─dbplyr:::collect.tbl_sql(x)
        12. │             └─dbplyr::db_sql_render(x$con, x, cte = cte)
        13. │               ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        14. │               └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        15. │                 ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        16. │                 └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        17. │                   ├─dbplyr::sql_render(...)
        18. │                   └─dbplyr:::sql_render.lazy_query(...)
        19. │                     ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        20. │                     └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        21. │                       └─dbplyr:::get_select_sql(...)
        22. │                         └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        23. │                           └─base::lapply(...)
        24. │                             └─dbplyr (local) FUN(X[[i]], ...)
        25. │                               ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        26. │                               └─rlang::eval_tidy(x, mask)
        27. └─dplyr::n_distinct(subject_id)
        28.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        29.     └─utils::getFromNamespace(fun, pkg)
        30.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 8 | WARN 0 | SKIP 101 | PASS 0 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      24. │                             └─dbplyr (local) FUN(X[[i]], ...)
      25. │                               ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      26. │                               └─rlang::eval_tidy(x, mask)
      27. └─dplyr::n_distinct(subject_id)
      28.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      29.     └─utils::getFromNamespace(fun, pkg)
      30.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'a06_Working_with_IncidencePrevalence_Results.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘a06_Working_with_IncidencePrevalence_Results.Rmd’
     
     --- re-building ‘a07_benchmark.Rmd’ using rmarkdown
     --- finished re-building ‘a07_benchmark.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘a01_Introduction_to_IncidencePrevalence.Rmd’
       ‘a02_Creating_denominator_populations.Rmd’
       ‘a03_Creating_target_denominator_populations.Rmd’
       ‘a04_Calculating_prevalence.Rmd’ ‘a05_Calculating_incidence.Rmd’
       ‘a06_Working_with_IncidencePrevalence_Results.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# janitor (2.2.1)

* GitHub: <https://github.com/sfirke/janitor>
* Email: <mailto:samuel.firke@gmail.com>
* GitHub mirror: <https://github.com/cran/janitor>

Run `revdepcheck::cloud_details(, "janitor")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
       • On CRAN (1): 'test-adorn-rounding.R:25:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-clean-names.R:623:5'): tbl_lazy/dbplyr ─────────────────────────
       Error in `as_table_path(name, dest$con)`: `name` uses unknown specification for table name
       Backtrace:
            ▆
         1. ├─... %>% ... at test-clean-names.R:623:5
         2. ├─dplyr::select(...)
         3. ├─dplyr::mutate(., repeated_2 = repeated, x = NA)
         4. └─dbplyr::memdb_frame(test_df %>% dplyr::select(-"*", -REPEATED))
         5.   ├─dplyr::copy_to(memdb(), tibble(...), name = .name)
         6.   └─dplyr:::copy_to.DBIConnection(memdb(), tibble(...), name = .name)
         7.     ├─dplyr::copy_to(...)
         8.     └─dbplyr:::copy_to.src_sql(...)
         9.       └─dbplyr::as_table_path(name, dest$con)
        10.         └─cli::cli_abort(...)
        11.           └─rlang::abort(...)
       
       [ FAIL 1 | WARN 10 | SKIP 1 | PASS 802 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# lazysf (0.2.0)

* GitHub: <https://github.com/hypertidy/lazysf>
* Email: <mailto:mdsumner@gmail.com>
* GitHub mirror: <https://github.com/cran/lazysf>

Run `revdepcheck::cloud_details(, "lazysf")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ! Failed to find table "nc.gpkg".
     ℹ Did you mean `from = I("nc.gpkg")`?
     Caused by error in `dbplyr_query_fields()`:
     ! <SFSQLConnection> uses dbplyr's 1st edition interface, which is no
       longer supported.
     ℹ Please contact the maintainer of the package for a solution.
     Backtrace:
          ▆
       1. ├─lazysf::lazysf(f)
       2. └─lazysf:::lazysf.character(f)
       3.   ├─lazysf::lazysf(db, layer, ..., query = query)
       4.   └─lazysf:::lazysf.SFSQLConnection(db, layer, ..., query = query)
       5.     ├─dplyr::tbl(x, layer)
       6.     └─dplyr:::tbl.DBIConnection(x, layer)
       7.       ├─dplyr::tbl(...)
       8.       └─dbplyr:::tbl.src_dbi(...)
       9.         └─dbplyr:::db_table(src$con, from, vars = vars)
      10.           ├─vars %||% find_variables(con, from, call = call)
      11.           └─dbplyr:::find_variables(con, from, call = call)
      12.             ├─base::withCallingHandlers(...)
      13.             └─dbplyr:::dbplyr_query_fields(con, source)
      14.               └─dbplyr:::check_2ed(con)
      15.                 └─cli::cli_abort(...)
      16.                   └─rlang::abort(...)
     Execution halted
     ```

# mlr3db (0.7.0)

* GitHub: <https://github.com/mlr-org/mlr3db>
* Email: <mailto:marcbecker@posteo.de>
* GitHub mirror: <https://github.com/cran/mlr3db>

Run `revdepcheck::cloud_details(, "mlr3db")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        62. │                                   ├─dplyr::collect(...)
        63. │                                   └─dbplyr:::collect.tbl_sql(...)
        64. │                                     ├─base::withCallingHandlers(...)
        65. │                                     ├─dbplyr::db_collect(...)
        66. │                                     └─dbplyr:::db_collect.DBIConnection(...)
        67. │                                       ├─DBI::dbSendQuery(con, sql)
        68. │                                       └─DBI::dbSendQuery(con, sql)
        69. │                                         └─RSQLite (local) .local(conn, statement, ...)
        70. │                                           ├─methods::new(...)
        71. │                                           │ ├─methods::initialize(value, ...)
        72. │                                           │ └─methods::initialize(value, ...)
        73. │                                           └─RSQLite:::result_create(conn@ptr, statement)
        74. └─base::.handleSimpleError(`<fn>`, "bad_weak_ptr", base::quote(NULL))
        75.   └─dbplyr (local) h(simpleError(msg, call))
        76.     └─cli::cli_abort("Failed to collect lazy table.", parent = cnd)
        77.       └─rlang::abort(...)
       
       [ FAIL 3 | WARN 1 | SKIP 3 | PASS 1133 ]
       Error:
       ! Test failures.
       Warning messages:
       1: call dbDisconnect() when finished working with a connection 
       2: In .Internal(gc(verbose, reset, full)) :
         closing unused connection 4 (<-localhost:22532)
       Execution halted
     ```

# motherduck (0.2.1)

* Email: <mailto:alejandro.hagan@outlook.com>
* GitHub mirror: <https://github.com/cran/motherduck>

Run `revdepcheck::cloud_details(, "motherduck")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       information
       
       -- Action Report: --
       
       v Inserted into existing database "memory"
       v Using existing schema "main"
       v Overwrite existing table "mtcars_excel"
       [ FAIL 1 | WARN 0 | SKIP 1 | PASS 20 ]
       
       ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
       • empty test (1): 'test-motherduck.R:259:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-motherduck.R:214:3'): create_table_dbi ─────────────────────────
       Error in `create_table(.data = out, .con = con_db, database_name = "memory", schema_name = "main", table_name = "dbi_overwrite", write_type = "overwrite")`: data must be either `tbl_dbi` or `data.frame` not tbl_duckdb_connection, tbl_sql, tbl_lazy, and tbl
       Backtrace:
           ▆
        1. └─motherduck::create_table(...) at test-motherduck.R:214:3
        2.   └─cli::cli_abort("data must be either {.var tbl_dbi} or {.var data.frame} not {data_class}")
        3.     └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 1 | PASS 20 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# osdc (0.9.19)

* GitHub: <https://github.com/steno-aarhus/osdc>
* Email: <mailto:lwjohnst@gmail.com>
* GitHub mirror: <https://github.com/cran/osdc>

Run `revdepcheck::cloud_details(, "osdc")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > # * https://testthat.r-lib.org/articles/special-files.html
       > 
       > library(testthat)
       > library(osdc)
       > 
       > test_check("osdc")
       Saving _problems/test-classify-diabetes-36.R
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 43 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-classify-diabetes.R:25:1'): (code run outside of `test_that()`) ──
       Error in `dplyr::collect(classify_diabetes(kontakter = cases_vs_nc$kontakter, diagnoser = cases_vs_nc$diagnoser, lpr_diag = cases_vs_nc$lpr_diag, lpr_adm = cases_vs_nc$lpr_adm, sysi = cases_vs_nc$sysi, sssy = cases_vs_nc$sssy, lab_forsker = cases_vs_nc$lab_forsker, bef = cases_vs_nc$bef, lmdb = cases_vs_nc$lmdb))`: Failed to collect lazy table.
       Caused by error in `DBI::dbSendQuery()`:
       ! Catalog Error: Table with name as_tbl_duckplyr_XMUENgxduz does not exist!
       Did you mean "as_tbl_duckplyr_pwmXguVtkD"?
       
       LINE 208:                     FROM as_tbl_duckplyr_XMUENgxduz
                                          ^
       i Context: rapi_prepare
       i Error type: CATALOG
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 43 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# PatientProfiles (1.4.5)

* GitHub: <https://github.com/darwin-eu/PatientProfiles>
* Email: <mailto:marti.catalasabate@ndorms.ox.ac.uk>
* GitHub mirror: <https://github.com/cran/PatientProfiles>

Run `revdepcheck::cloud_details(, "PatientProfiles")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        28.       └─base::get(x, envir = ns, inherits = FALSE)
       ── Failure ('test-filterInObservation.R:67:3'): test filterInObservation ───────
       Expected `... <- NULL` not to throw any errors.
       Actually got a <rlang_error> with message:
         Failed to collect lazy table.
         Caused by error in `DBI::dbSendQuery()`:
         ! Binder Error: Referenced column "id_wex" not found in FROM clause!
         Candidate bindings: "drug_exposure_id", "drug_exposure_end_date", "drug_exposure_start_date", "drug_exposure_end_datetime", "drug_exposure_start_datetime"
         
         LINE 5: WHERE (id_wex <= drug_exposure_start_date AND drug_exposure_start_...
                        ^
         i Context: rapi_prepare
         i Error type: BINDER
       ── Error ('test-filterInObservation.R:73:3'): test filterInObservation ─────────
       Error in `eval(code, test_env)`: object 'xS' not found
       Backtrace:
           ▆
        1. └─testthat::expect_identical(xS, c(1L, 4L)) at test-filterInObservation.R:73:3
        2.   └─testthat::quasi_label(enquo(object), label)
        3.     └─rlang::eval_bare(expr, quo_get_env(quo))
       
       [ FAIL 3 | WARN 61 | SKIP 109 | PASS 189 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      16. │                 ├─dbplyr::sql_render(...)
      17. │                 └─dbplyr:::sql_render.lazy_query(...)
      18. │                   ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
      19. │                   └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
      20. │                     └─dbplyr:::get_select_sql(...)
      21. │                       └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
      22. │                         └─base::lapply(...)
      23. │                           └─dbplyr (local) FUN(X[[i]], ...)
      24. │                             ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      25. │                             └─rlang::eval_tidy(x, mask)
      26. └─dplyr::n_distinct(subject_id)
      27.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      28.     └─utils::getFromNamespace(fun, pkg)
      29.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'table-intersect.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘table-intersect.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘concept-intersect.Rmd’ ‘summarise.Rmd’ ‘table-intersect.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# pool (1.0.4)

* GitHub: <https://github.com/rstudio/pool>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/pool>

Run `revdepcheck::cloud_details(, "pool")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > ### ** Examples
     > 
     > library(dplyr)
     
     Attaching package: ‘dplyr’
     
     The following objects are masked from ‘package:stats’:
     
         filter, lag
     
     The following objects are masked from ‘package:base’:
     
         intersect, setdiff, setequal, union
     
     > 
     > pool <- dbPool(RSQLite::SQLite())
     > # copy a table into the database
     > copy_to(pool, mtcars, "mtcars", temporary = FALSE)
     Error in UseMethod("sql_dialect") : 
       no applicable method for 'sql_dialect' applied to an object of class "c('Pool', 'R6')"
     Calls: copy_to ... as_table_path -> make_table_path -> sql_escape_ident -> sql_dialect
     Execution halted
     <pool> Checked-out object deleted before being returned.
     <pool> Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`
     ```

*   checking tests ... ERROR
     ```
     ...
         5.       └─dbplyr:::db_table(src$con, from, vars = vars, subclass = subclass)
         6.         └─dbplyr:::as_table_source(from, con = con, error_call = call)
         7.           └─dbplyr::as_table_path(x, con = con, error_arg = error_arg, error_call = error_call)
         8.             └─dbplyr:::make_table_path(x, con, collapse = FALSE)
         9.               └─dbplyr::sql_escape_ident(con, x[needs_quote])
        10.                 └─dbplyr::sql_dialect(con)
       ── Error ('test-dbplyr.R:73:3'): can use schemas with pool ─────────────────────
       Error in `UseMethod("sql_dialect")`: no applicable method for 'sql_dialect' applied to an object of class "c('Pool', 'R6')"
       Backtrace:
            ▆
         1. ├─dplyr::copy_to(...) at test-dbplyr.R:73:3
         2. └─pool:::copy_to.Pool(...)
         3.   └─pool:::tbl.Pool(dest, name)
         4.     └─dbplyr::tbl_sql("Pool", dbplyr::src_dbi(src), from, ..., vars = vars)
         5.       └─dbplyr:::db_table(src$con, from, vars = vars, subclass = subclass)
         6.         └─dbplyr:::as_table_source(from, con = con, error_call = call)
         7.           └─dbplyr::as_table_path(x, con = con, error_arg = error_arg, error_call = error_call)
         8.             └─dbplyr:::make_table_path(list(x$schema, x$table), con)
         9.               └─dbplyr::sql_escape_ident(con, x[needs_quote])
        10.                 └─dbplyr::sql_dialect(con)
       
       [ FAIL 5 | WARN 5 | SKIP 19 | PASS 51 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# RClickhouse (0.6.10)

* GitHub: <https://github.com/IMSMWU/RClickhouse>
* Email: <mailto:christian.hotz-behofsits@wu.ac.at>
* GitHub mirror: <https://github.com/cran/RClickhouse>

Run `revdepcheck::cloud_details(, "RClickhouse")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         7.       └─dbplyr:::dbplyr_sql_translation(con)
         8.         └─dbplyr:::check_2ed(con)
         9.           └─cli::cli_abort(...)
        10.             └─rlang::abort(...)
       ── Error ('test-prefix.R:13:5'): custom aggregators translated correctly ───────
       Error in `dbplyr_sql_translation(con)`: <ClickhouseConnection> uses dbplyr's 1st edition interface, which is no
       longer supported.
       ℹ Please contact the maintainer of the package for a solution.
       Backtrace:
            ▆
         1. ├─testthat::expect_equal(trans(CaseSensitive(x)), sql("CaseSensitive(`x`)")) at test-prefix.R:16:3
         2. │ └─testthat::quasi_label(enquo(object), label)
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. └─RClickhouse (local) trans(CaseSensitive(x))
         5.   └─dbplyr::translate_sql(!!enquo(x), window = FALSE, con = simulate_clickhouse()) at test-prefix.R:13:5
         6.     └─dbplyr::translate_sql_(...)
         7.       └─dbplyr:::dbplyr_sql_translation(con)
         8.         └─dbplyr:::check_2ed(con)
         9.           └─cli::cli_abort(...)
        10.             └─rlang::abort(...)
       
       [ FAIL 2 | WARN 0 | SKIP 25 | PASS 4 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# RPresto (1.4.8)

* GitHub: <https://github.com/prestodb/RPresto>
* Email: <mailto:jarodm@fb.com>
* GitHub mirror: <https://github.com/cran/RPresto>

Run `revdepcheck::cloud_details(, "RPresto")` for more info

## Newly broken

*   checking whether package ‘RPresto’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/RPresto/new/RPresto.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘RPresto’ ...
** this is package ‘RPresto’ version ‘1.4.8’
** package ‘RPresto’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘sql_optimise’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘RPresto’
* removing ‘/tmp/workdir/RPresto/new/RPresto.Rcheck/RPresto’


```
### CRAN

```
* installing *source* package ‘RPresto’ ...
** this is package ‘RPresto’ version ‘1.4.8’
** package ‘RPresto’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RPresto)


```
# SCDB (0.5.2)

* GitHub: <https://github.com/ssi-dk/SCDB>
* Email: <mailto:rske@ssi.dk>
* GitHub mirror: <https://github.com/cran/SCDB>

Run `revdepcheck::cloud_details(, "SCDB")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### ** Examples
     > 
     > ## Don't show: 
     > if (requireNamespace("RSQLite", quietly = TRUE)) withAutoprint({ # examplesIf
     + ## End(Don't show)
     +   conn <- get_connection()
     + 
     +   mt <- dplyr::copy_to(conn, dplyr::distinct(mtcars, .data$mpg, .data$cyl), name = "mtcars")
     +   create_index(conn, mt, c("mpg", "cyl"))
     + 
     +   close_connection(conn)
     + ## Don't show: 
     + }) # examplesIf
     > conn <- get_connection()
     > mt <- dplyr::copy_to(conn, dplyr::distinct(mtcars, .data$mpg, .data$cyl), 
     +     name = "mtcars")
     > create_index(conn, mt, c("mpg", "cyl"))
     Error: Assertion failed. One of the following must apply:
      * checkmate::check_character(id): Must be of type 'character', not
      * 'tbl_SQLiteConnection/tbl_sql/tbl_lazy/tbl'
      * checkmate::check_class(id): Must inherit from class 'Id', but has
      * classes 'tbl_SQLiteConnection','tbl_sql','tbl_lazy','tbl'
      * checkmate::check_class(id): Must inherit from class 'tbl_dbi', but
      * has classes 'tbl_SQLiteConnection','tbl_sql','tbl_lazy','tbl'
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        10.       └─checkmate:::mstop(...)
       ── Failure ('test-update_snapshot.R:577:5'): update_snapshot() works with across connection ──
       Expected `update_snapshot(...)` not to throw any errors.
       Actually got a <simpleError> with message:
         Assertion on '.data' failed: Must inherit from class 'tbl_dbi', but has classes 'tbl_SQLiteConnection','tbl_sql','tbl_lazy','tbl'.
       ── Error ('test-update_snapshot.R:588:5'): update_snapshot() works with across connection ──
       Error: Table main.test.mtcars_modified could not be found!
       Backtrace:
           ▆
        1. ├─... %>% as.data.frame() at test-update_snapshot.R:588:5
        2. ├─base::as.data.frame(.)
        3. ├─dplyr::summarise(., dplyr::across(tidyselect::everything(), ~class(.)[1]))
        4. ├─dplyr::collect(.)
        5. └─SCDB::get_table(target_conn, target_table)
        6.   └─base::tryCatch(...)
        7.     └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        8.       └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        9.         └─value[[3L]](cond)
       
       [ FAIL 56 | WARN 0 | SKIP 1 | PASS 328 ]
       Error:
       ! Test failures.
       Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      1. └─global update_snapshot(...)
      2.   └─SCDB::update_snapshot(logger = LoggerNull$new(), ...)
      3.     └─checkmate::assert_class(.data, "tbl_dbi")
      4.       └─checkmate::makeAssertion(x, res, .var.name, add)
      5.         └─checkmate:::mstop(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'SCDB.Rmd' failed with diagnostics:
     Assertion on '.data' failed: Must inherit from class 'tbl_dbi', but has classes 'tbl_SQLiteConnection','tbl_sql','tbl_lazy','tbl'.
     --- failed re-building ‘SCDB.Rmd’
     
     --- re-building ‘benchmarks.Rmd’ using rmarkdown
     Loading required namespace: here
     fatal: not a git repository (or any of the parent directories): .git
     --- finished re-building ‘benchmarks.Rmd’
     
     --- re-building ‘slowly-changing-dimension.Rmd’ using rmarkdown
     Loading required namespace: tidyverse
     --- finished re-building ‘slowly-changing-dimension.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘SCDB.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# sergeant (0.9.1)

* Email: <mailto:bob@rud.is>
* GitHub mirror: <https://github.com/cran/sergeant>

Run `revdepcheck::cloud_details(, "sergeant")` for more info

## Newly broken

*   checking whether package ‘sergeant’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/sergeant/new/sergeant.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘sergeant’ ...
** this is package ‘sergeant’ version ‘0.9.1’
** package ‘sergeant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘src_sql’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘sergeant’
* removing ‘/tmp/workdir/sergeant/new/sergeant.Rcheck/sergeant’


```
### CRAN

```
* installing *source* package ‘sergeant’ ...
** this is package ‘sergeant’ version ‘0.9.1’
** package ‘sergeant’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sergeant)


```
# starwarsdb (0.1.3)

* GitHub: <https://github.com/gadenbuie/starwarsdb>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/starwarsdb>

Run `revdepcheck::cloud_details(, "starwarsdb")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘starwarsdb-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: starwars_dm
     > ### Title: Create a Star Wars Data Model Object
     > ### Aliases: starwars_dm starwars_dm_configure
     > 
     > ### ** Examples
     > 
     > # If the {dm} package is installed...
     > if (requireNamespace("dm", quietly = TRUE)) {
     +   # Create a full starwars {dm} object from local tables
     +   starwars_dm(remote = TRUE)
     + 
     +   # Create a base starwars {dm} object from remote tables wihout keys
     +   starwars_dm(configure_dm = FALSE, remote = TRUE)
     + }
     Error in UseMethod("src_tbls") : 
       no applicable method for 'src_tbls' applied to an object of class "c('src_duckdb_connection', 'src_dbi', 'src_sql', 'src')"
     Calls: starwars_dm ... <Anonymous> -> dm_from_con -> get_src_tbl_names -> src_tbls
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-dm.R:33:3'): starwars_dm remote full ───────────────────────────
       Error in `UseMethod("src_tbls")`: no applicable method for 'src_tbls' applied to an object of class "c('src_duckdb_connection', 'src_dbi', 'src_sql', 'src')"
       Backtrace:
           ▆
        1. └─starwarsdb::starwars_dm(remote = TRUE) at test-dm.R:33:3
        2.   └─dm::dm_from_src(starwars_connect(), learn_keys = FALSE)
        3.     └─dm::dm_from_con(...)
        4.       └─dm:::get_src_tbl_names(src, ..., names = .names)
        5.         └─dplyr::src_tbls(src)
       ── Error ('test-dm.R:53:3'): starwars_dm remote unconfigured ───────────────────
       Error in `UseMethod("src_tbls")`: no applicable method for 'src_tbls' applied to an object of class "c('src_duckdb_connection', 'src_dbi', 'src_sql', 'src')"
       Backtrace:
           ▆
        1. └─starwarsdb::starwars_dm(remote = TRUE, configure_dm = FALSE) at test-dm.R:53:3
        2.   └─dm::dm_from_src(starwars_connect(), learn_keys = FALSE)
        3.     └─dm::dm_from_con(...)
        4.       └─dm:::get_src_tbl_names(src, ..., names = .names)
        5.         └─dplyr::src_tbls(src)
       
       [ FAIL 2 | WARN 1 | SKIP 0 | PASS 24 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# taxadb (0.2.1)

* GitHub: <https://github.com/ropensci/taxadb>
* Email: <mailto:cboettig@gmail.com>
* GitHub mirror: <https://github.com/cran/taxadb>

Run `revdepcheck::cloud_details(, "taxadb")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       LINE 40: WHERE (REGEXP_MATCHES("input", gsub('%woodpecker%', '%', '')))
                                               ^
       ℹ Context: rapi_prepare
       ℹ Error type: CATALOG
       ── Error ('test-fuzzy_filter.R:23:3'): we can fuzzy match scientific and common names ──
       Error in `dplyr::collect(out)`: Failed to collect lazy table.
       Caused by error in `DBI::dbSendQuery()`:
       ! Catalog Error: Scalar Function with name gsub does not exist!
       Did you mean "generate_subscripts"?
       
       LINE 40: WHERE (REGEXP_MATCHES("input", gsub('%woodpecker%', '%', '')))
                                               ^
       ℹ Context: rapi_prepare
       ℹ Error type: CATALOG
       ── Failure ('test-handling-duplicates.R:26:3'): take_first_duplicate works in db connection ──
       Expected `nrow(out)` to equal 2.
       Differences:
       1/1 mismatches
       [1] NA - 2 == NA
       
       [ FAIL 3 | WARN 0 | SKIP 3 | PASS 56 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘parse_schema.R’
     ```

# tidier (0.2.0)

* GitHub: <https://github.com/talegari/tidier>
* Email: <mailto:sri.teach@gmail.com>
* GitHub mirror: <https://github.com/cran/tidier>

Run `revdepcheck::cloud_details(, "tidier")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +           .by = Month,
     +           .order_by = date_col,
     +           .frame = c(3, 3)
     +           ) %>%
     +    dplyr::collect() %>%
     +    dplyr::select(Ozone, Solar.R, Wind, Temp, Month, Day, date_col, avg_temp)
     Error in `as_table_path()`:
     ! `name` uses unknown specification for table name
     Backtrace:
          ▆
       1. ├─... %>% ...
       2. ├─dplyr::select(...)
       3. ├─dplyr::collect(.)
       4. ├─tidier::mutate(...)
       5. │ └─checkmate::assert_multi_class(x, c("data.frame", "tbl_lazy"))
       6. │   └─checkmate::checkMultiClass(x, classes, null.ok)
       7. └─dbplyr::memdb_frame(.)
       8.   ├─dplyr::copy_to(memdb(), tibble(...), name = .name)
       9.   └─dplyr:::copy_to.DBIConnection(memdb(), tibble(...), name = .name)
      10.     ├─dplyr::copy_to(...)
      11.     └─dbplyr:::copy_to.src_sql(...)
      12.       └─dbplyr::as_table_path(name, dest$con)
      13.         └─cli::cli_abort(...)
      14.           └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('tests_tidier.R:287:3'): compare mutate df vs sb ─────────────────────
       Error in `as_table_path(name, dest$con)`: `name` uses unknown specification for table name
       Backtrace:
            ▆
         1. ├─... %>% ... at tests_tidier.R:287:3
         2. ├─dplyr::select(...)
         3. ├─dplyr::collect(.)
         4. ├─tidier::mutate(...)
         5. │ └─checkmate::assert_multi_class(x, c("data.frame", "tbl_lazy"))
         6. │   └─checkmate::checkMultiClass(x, classes, null.ok)
         7. └─dbplyr::memdb_frame(.)
         8.   ├─dplyr::copy_to(memdb(), tibble(...), name = .name)
         9.   └─dplyr:::copy_to.DBIConnection(memdb(), tibble(...), name = .name)
        10.     ├─dplyr::copy_to(...)
        11.     └─dbplyr:::copy_to.src_sql(...)
        12.       └─dbplyr::as_table_path(name, dest$con)
        13.         └─cli::cli_abort(...)
        14.           └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 18 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# TreatmentPatterns (3.1.1)

* GitHub: <https://github.com/darwin-eu/TreatmentPatterns>
* Email: <mailto:m.l.vankessel@erasmusmc.nl>
* GitHub mirror: <https://github.com/cran/TreatmentPatterns>

Run `revdepcheck::cloud_details(, "TreatmentPatterns")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        11. │           └─dbplyr:::collect.tbl_sql(x)
        12. │             └─dbplyr::db_sql_render(x$con, x, cte = cte)
        13. │               ├─dbplyr:::db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
        14. │               └─dbplyr:::db_sql_render.DBIConnection(con, sql, ..., sql_options = sql_options)
        15. │                 ├─dbplyr::sql_render(sql, con = con, ..., sql_options = sql_options)
        16. │                 └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., sql_options = sql_options)
        17. │                   ├─dbplyr::sql_render(...)
        18. │                   └─dbplyr:::sql_render.lazy_query(...)
        19. │                     ├─dbplyr::sql_build(query, con = con, sql_options = sql_options)
        20. │                     └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
        21. │                       └─dbplyr:::get_select_sql(...)
        22. │                         └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
        23. │                           └─base::lapply(...)
        24. │                             └─dbplyr (local) FUN(X[[i]], ...)
        25. │                               ├─dbplyr::escape(eval_tidy(x, mask), con = con)
        26. │                               └─rlang::eval_tidy(x, mask)
        27. └─dplyr::n_distinct(subject_id)
        28.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
        29.     └─utils::getFromNamespace(fun, pkg)
        30.       └─base::get(x, envir = ns, inherits = FALSE)
       
       [ FAIL 2 | WARN 0 | SKIP 119 | PASS 0 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      19. │         └─dbplyr:::sql_build.lazy_select_query(query, con = con, sql_options = sql_options)
      20. │           └─dbplyr:::get_select_sql(...)
      21. │             └─dbplyr::translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
      22. │               └─base::lapply(...)
      23. │                 └─dbplyr (local) FUN(X[[i]], ...)
      24. │                   ├─dbplyr::escape(eval_tidy(x, mask), con = con)
      25. │                   └─rlang::eval_tidy(x, mask)
      26. └─dplyr::n_distinct(subject_id)
      27.   └─duckdb:::pkg_method("glue_sql2", "dbplyr")
      28.     └─utils::getFromNamespace(fun, pkg)
      29.       └─base::get(x, envir = ns, inherits = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'a030_Evaluating_Output.Rmd' failed with diagnostics:
     object 'glue_sql2' not found
     --- failed re-building ‘a030_Evaluating_Output.Rmd’
     
     --- re-building ‘a999_Strategus.Rmd’ using rmarkdown
     --- finished re-building ‘a999_Strategus.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘a010_Computing_Pathways.Rmd’ ‘a030_Evaluating_Output.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

