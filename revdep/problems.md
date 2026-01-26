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

# CohortCharacteristics (1.1.0)

* GitHub: <https://github.com/darwin-eu/CohortCharacteristics>
* Email: <mailto:marti.catalasabate@ndorms.ox.ac.uk>
* GitHub mirror: <https://github.com/cran/CohortCharacteristics>

Run `revdepcheck::cloud_details(, "CohortCharacteristics")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        11. │           └─DBI::dbSendStatement(conn, statement, ...)
        12. │             ├─DBI::dbSendQuery(conn, statement, ...)
        13. │             └─duckdb::dbSendQuery(conn, statement, ...)
        14. │               └─duckdb (local) .local(conn, statement, ...)
        15. │                 └─duckdb:::rethrow_rapi_prepare(conn@conn_ref, statement, env)
        16. │                   ├─rlang::try_fetch(...)
        17. │                   │ ├─base::tryCatch(...)
        18. │                   │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        19. │                   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        20. │                   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        21. │                   │ └─base::withCallingHandlers(...)
        22. │                   └─duckdb:::rapi_prepare(conn, query, env)
        23. ├─duckdb (local) `<fn>`(...)
        24. │ └─rlang::abort(error_parts, class = "duckdb_error", !!!fields)
        25. │   └─rlang:::signal_abort(cnd, .file)
        26. │     └─base::signalCondition(cnd)
        27. └─rlang (local) `<fn>`(`<dckdb_rr>`)
        28.   └─handlers[[1L]](cnd)
        29.     └─duckdb:::rethrow_error_from_rapi(e, call)
        30.       └─rlang::abort(msg, call = call)
       
       [ FAIL 1 | WARN 17 | SKIP 24 | PASS 228 ]
       Error:
       ! Test failures.
       Execution halted
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
       <purrr_error_indexed/rlang_error/error/condition>
       Error in `map(.x, .f, ..., .progress = .progress)`: i In index: 1.
       Caused by error in `pmap()`:
       i In index: 1.
       Caused by error:
       ! no such table: `SCDB_digest_to_checksum_BrP2VHaKPx`
       ── Error ('test-drop_diseasystore.R:127:5'): drop_diseasystore can delete single table in default schema ──
       <purrr_error_indexed/rlang_error/error/condition>
       Error in `map(.x, .f, ..., .progress = .progress)`: i In index: 1.
       Caused by error in `pmap()`:
       i In index: 1.
       Caused by error:
       ! no such table: `SCDB_digest_to_checksum_KFxPNpSEmX`
       ── Failure ('test-zzz.R:12:3'): data is not written locally ────────────────────
       Expected `dir(recursive = TRUE)` to have the same values as `current_files`.
       Actual: "_problems/test-DiseasystoreBase-134.R", "_problems/test-DiseasystoreBase-290.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-drop_diseasystore-128.R", "_problems/test-drop_diseasystore-69.R", "helper-setup.R", "setup.R", "test-0_R6_utils.R", ...
       Expected: "helper-setup.R", "setup.R", "test-0_R6_utils.R", "test-0_documentation.R", "test-0_linters.R", "test-0_rd_files.R", "test-DiseasystoreBase.R", "test-DiseasystoreEcdcRespitoryViruses.R", "test-DiseasystoreGoogleCovid19.R", ...
       Needs: "_problems/test-DiseasystoreBase-134.R", "_problems/test-DiseasystoreBase-290.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-drop_diseasystore-128.R", "_problems/test-drop_diseasystore-69.R"
       
       [ FAIL 22 | WARN 5 | SKIP 12 | PASS 443 ]
       Error:
       ! Test failures.
       Warning message:
       In connection_release(conn@ptr) : Already disconnected
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
          ▆
       1. └─`<fn>`(`<Logger>`)
       2.   └─.subset2(e, ".__enclos_env__")$private$finalize()
       3.     └─self$finalize_db_entry()
       4.       └─dbplyr::build_sql(...)
       5.         └─purrr::map_chr(enexprs(...), escape_expr, con = con)
       6.           └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
       7.             ├─purrr:::with_indexed_errors(...)
       8.             │ └─base::withCallingHandlers(...)
       9.             ├─purrr:::call_with_cleanup(...)
      10.             └─dbplyr (local) .f(.x[[i]], ...)
      11.               ├─dbplyr::escape(val, con = con)
      12.               └─dbplyr:::escape.default(val, con = con)
      13.                 └─dbplyr:::error_embed(obj_type_friendly(x), "x")
      14.                   └─cli::cli_abort(...)
      15.                     └─rlang::abort(...)
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

# etl (0.4.2)

* GitHub: <https://github.com/beanumber/etl>
* Email: <mailto:ben.baumer@gmail.com>
* GitHub mirror: <https://github.com/cran/etl>

Run `revdepcheck::cloud_details(, "etl")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
           intersect, setdiff, setequal, union
       
       > 
       > test_check("etl")
       Saving _problems/test-etl-97.R
       [ FAIL 1 | WARN 1 | SKIP 3 | PASS 30 ]
       
       ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
       • On CRAN (2): 'test-etl.R:102:3', 'test-etl.R:119:3'
       • empty test (1): 'test-etl.R:51:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-etl.R:97:3'): etl works ──────────────────────────────────────
       Expected output from `print(cars)` to match regexp "sqlite".
       Actual output:
       ✖ │ dir:  2 files occupying 0 GB
         │ <S4 class 'SQLiteConnection' [package "RSQLite"] with 8 slots>NULL
       
       [ FAIL 1 | WARN 1 | SKIP 3 | PASS 30 ]
       Error:
       ! Test failures.
       Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

# healthdb (0.5.0)

* GitHub: <https://github.com/KevinHzq/healthdb>
* Email: <mailto:kevin.hu@bccdc.ca>
* GitHub mirror: <https://github.com/cran/healthdb>

Run `revdepcheck::cloud_details(, "healthdb")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
            ▆
         1. ├─healthdb:::identify_rows(...) at test-identify_rows_sql.R:88:3
         2. ├─healthdb:::identify_rows.tbl_sql(...)
         3. │ └─q_match %>% dplyr::collect()
         4. ├─dplyr::collect(.)
         5. ├─dbplyr:::collect.tbl_sql(.)
         6. │ ├─base::withCallingHandlers(...)
         7. │ ├─dbplyr::db_collect(...)
         8. │ └─dbplyr:::db_collect.DBIConnection(...)
         9. │   ├─DBI::dbSendQuery(con, sql)
        10. │   └─DBI::dbSendQuery(con, sql)
        11. │     └─RSQLite (local) .local(conn, statement, ...)
        12. │       ├─methods::new(...)
        13. │       │ ├─methods::initialize(value, ...)
        14. │       │ └─methods::initialize(value, ...)
        15. │       └─RSQLite:::result_create(conn@ptr, statement)
        16. └─base::.handleSimpleError(`<fn>`, "row value misused", base::quote(NULL))
        17.   └─dbplyr (local) h(simpleError(msg, call))
        18.     └─cli::cli_abort("Failed to collect lazy table.", parent = cnd)
        19.       └─rlang::abort(...)
       
       [ FAIL 13 | WARN 238 | SKIP 7 | PASS 262 ]
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
         closing unused connection 4 (<-localhost:33345)
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
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-filterInObservation.R:67:3'): test filterInObservation ───────
       Expected `... <- NULL` not to throw any errors.
       Actually got a <rlang_error> with message:
         Failed to collect lazy table.
         Caused by error in `DBI::dbSendQuery()`:
         ! Binder Error: Referenced column "id_vgz" not found in FROM clause!
         Candidate bindings: "sig", "visit_detail_id", "period_type_concept_id", "days_supply"
         
         LINE 5: WHERE (id_vgz <= drug_exposure_start_date AND drug_exposure_start_...
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
       
       [ FAIL 2 | WARN 61 | SKIP 109 | PASS 192 ]
       Error:
       ! Test failures.
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
     Error in UseMethod("sql_escape_ident", sql_dialect(con)) : 
       no applicable method for 'sql_escape_ident' applied to an object of class "c('Pool', 'R6')"
     Calls: copy_to ... as_table_path -> make_table_path -> sql_escape_ident
     Execution halted
     <pool> Checked-out object deleted before being returned.
     <pool> Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`
     ```

*   checking tests ... ERROR
     ```
     ...
        3.   └─pool:::tbl.Pool(dest, name)
        4.     └─dbplyr::tbl_sql("Pool", dbplyr::src_dbi(src), from, ..., vars = vars)
        5.       └─dbplyr:::db_table(src$con, from, vars = vars, subclass = subclass)
        6.         └─dbplyr:::as_table_source(from, con = con, error_call = call)
        7.           └─dbplyr::as_table_path(x, con = con, error_arg = error_arg, error_call = error_call)
        8.             └─dbplyr:::make_table_path(x, con, collapse = FALSE)
        9.               └─dbplyr::sql_escape_ident(con, x[needs_quote])
       ── Error ('test-dbplyr.R:73:3'): can use schemas with pool ─────────────────────
       Error in `UseMethod("sql_escape_ident", sql_dialect(con))`: no applicable method for 'sql_escape_ident' applied to an object of class "c('Pool', 'R6')"
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
       
       [ FAIL 5 | WARN 0 | SKIP 19 | PASS 51 ]
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

*   checking tests ... ERROR
     ```
     ...
         'test-presto_type.R:383:3', 'test-presto_type.R:420:3',
         'test-presto_type.R:478:3', 'test-presto_unnest.R:13:3',
         'test-presto_unnest.R:49:3', 'test-presto_unnest.R:84:3',
         'test-presto_unnest.R:115:3', 'test-presto_unnest.R:144:3',
         'test-presto_unnest.R:173:3', 'test-presto_unnest.R:207:3',
         'test-presto_unnest.R:240:3', 'test-presto_unnest.R:273:3',
         'test-presto_unnest.R:312:3', 'test-presto_unnest.R:354:3',
         'test-session.property.R:19:3', 'test-session.timezone.R:21:3',
         'test-sqlAppendTableAs.R:10:3', 'test-sqlAppendTableAs.R:39:3',
         'test-sqlAppendTableAs.R:67:3', 'test-sqlAppendTableAs.R:99:3',
         'test-sqlAppendTableAs.R:130:3', 'test-tbl.src_presto.R:10:3',
         'test-tbl.src_presto.R:44:3', 'test-tbl.src_presto.R:55:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-db_query_fields.R:42:7'): db_query_fields works with mock ──────
       Error in `UseMethod("db_query_fields")`: no applicable method for 'db_query_fields' applied to an object of class "c('PrestoConnection', 'DBIConnection', 'DBIObject')"
       Backtrace:
           ▆
        1. ├─testthat::with_mocked_bindings(...) at test-db_query_fields.R:40:3
        2. └─dplyr::db_query_fields(...) at test-db_query_fields.R:42:7
       
       [ FAIL 1 | WARN 0 | SKIP 164 | PASS 209 ]
       Error:
       ! Test failures.
       Execution halted
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
     + 
     +   dplyr::tbl(conn, "test.mtcars")
     + 
     +   close_connection(conn)
     + ## Don't show: 
     + }) # examplesIf
     > conn <- get_connection()
     > data <- dplyr::copy_to(conn, mtcars)
     > update_snapshot(head(data, 3), conn = conn, db_table = "test.mtcars", 
     +     timestamp = Sys.time())
     Warning: `log_path` and `log_tbl` are both `NULL` and therefore NO file or database logging will be done.
     Consider adding options SCDB.log_table_id and/or SCDB.log_path to your .Rprofile
     2026-01-23 15:10:28.382 - root - INFO - Started
     2026-01-23 15:10:28.382 - root - INFO - Parsing data for table main.test.mtcars started
     2026-01-23 15:10:28.382 - root - INFO - Given timestamp for table is 2026-01-23 15:10:28.382749
     2026-01-23 15:10:28.583 - root - INFO - Calculated checksums
     2026-01-23 15:10:28.693 - root - INFO - Deactivating records
     Warning: `as.sql()` was deprecated in dbplyr 2.6.0.
     ℹ Please use `as_table_path()` instead.
     ℹ The deprecated feature was likely used in the SCDB package.
       Please report the issue at <https://github.com/ssi-dk/SCDB/issues>.
     2026-01-23 15:10:28.776 - root - INFO - Deactivate records count: 0
     2026-01-23 15:10:28.776 - root - INFO - Adding new records
     Error: no such table: `SCDB_digest_to_checksum_Ry23f5MYPf`
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Warning message:
       call dbDisconnect() when finished working with a connection 
       Execution halted
       Error in `purrr::map_chr()`:
       ℹ In index: 2.
       Caused by error:
       ! Cannot translate a <Id> object to SQL.
       ℹ Do you want to force evaluation in R with (e.g.) `!!x` or `local(x)`?
       Backtrace:
            ▆
         1. └─`<fn>`(`<Logger>`)
         2.   └─.subset2(e, ".__enclos_env__")$private$finalize()
         3.     └─self$finalize_db_entry()
         4.       └─dbplyr::build_sql(...)
         5.         └─purrr::map_chr(enexprs(...), escape_expr, con = con)
         6.           └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
         7.             ├─purrr:::with_indexed_errors(...)
         8.             │ └─base::withCallingHandlers(...)
         9.             ├─purrr:::call_with_cleanup(...)
        10.             └─dbplyr (local) .f(.x[[i]], ...)
        11.               ├─dbplyr::escape(val, con = con)
        12.               └─dbplyr:::escape.default(val, con = con)
        13.                 └─dbplyr:::error_embed(obj_type_friendly(x), "x")
        14.                   └─cli::cli_abort(...)
        15.                     └─rlang::abort(...)
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      5.       └─RSQLite (local) .local(conn, statement, ...)
      6.         ├─methods::new(...)
      7.         │ ├─methods::initialize(value, ...)
      8.         │ └─methods::initialize(value, ...)
      9.         └─RSQLite:::result_create(conn@ptr, statement)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'SCDB.Rmd' failed with diagnostics:
     no such table: `SCDB_digest_to_checksum_qs6CYZ5vrf`
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

