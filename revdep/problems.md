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

# diseasystore (0.3.3)

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
       ! 2026-05-11 22:30:29.895 - root - ERROR - Columns do not match!
        Table columns:
        key_car, n_cyl, valid_from, valid_until 
       Input columns:
        key_car, n_cyl, valid_from, valid_until, cyl
       ── Failure ('test-zzz.R:12:3'): data is not written locally ────────────────────
       Expected `dir(recursive = TRUE)` to have the same values as `current_files`.
       Actual: "_problems/test-DiseasystoreBase-162.R", "_problems/test-DiseasystoreBase-241.R", "_problems/test-DiseasystoreBase-408.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-drop_diseasystore-128.R", "_problems/test-drop_diseasystore-69.R", "helper-setup.R", "setup.R", ...
       Expected: "helper-setup.R", "setup.R", "test-0_R6_utils.R", "test-0_documentation.R", "test-0_linters.R", "test-0_rd_files.R", "test-DiseasystoreBase.R", "test-DiseasystoreEcdcRespitoryViruses.R", "test-DiseasystoreGoogleCovid19.R", ...
       Needs: "_problems/test-DiseasystoreBase-162.R", "_problems/test-DiseasystoreBase-241.R", "_problems/test-DiseasystoreBase-408.R", "_problems/test-DiseasystoreEcdcRespitoryViruses-14.R", "_problems/test-DiseasystoreGoogleCovid19-11.R", "_problems/test-drop_diseasystore-128.R", "_problems/test-drop_diseasystore-69.R"
       
       [ FAIL 24 | WARN 37 | SKIP 12 | PASS 447 ]
       Error:
       ! Test failures.
       Warning messages:
       1: In connection_release(conn@ptr) : Already disconnected
       2: call dbDisconnect() when finished working with a connection 
       3: In connection_release(conn@ptr) : Already disconnected
       4: In connection_release(conn@ptr) : Already disconnected
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      25.                     └─diseasystore (local) .f(start_date = .l[[1L]][[i]], end_date = .l[[2L]][[i]], ...)
      26.                       └─SCDB::update_snapshot(...)
      27.                         ├─DBI::dbSendQuery(conn, sql_insert)
      28.                         └─DBI::dbSendQuery(conn, sql_insert)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'extending-diseasystore-example.Rmd' failed with diagnostics:
     ℹ In index: 1.
     Caused by error in `pmap()`:
     ℹ In index: 1.
     Caused by error in `DBI::dbSendQuery()`:
     ! Binder Error: Cannot extract field 'checksum' from expression "'dbplyr_tmp_J8NF0eV2TV'" because it is not a struct, union, map, or json
     ℹ Context: rapi_prepare
     ℹ Error type: BINDER
     --- failed re-building ‘extending-diseasystore-example.Rmd’
     
     Warning: Connection already closed.
     --- re-building ‘extending-diseasystore.Rmd’ using rmarkdown
     --- finished re-building ‘extending-diseasystore.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘extending-diseasystore-example.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# dm (1.1.1)

* GitHub: <https://github.com/cynkra/dm>
* Email: <mailto:kirill@cynkra.com>
* GitHub mirror: <https://github.com/cran/dm>

Run `revdepcheck::cloud_details(, "dm")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       5. │     └─base::eval(ei, envir)
       6. ├─dm::copy_dm_to(...)
       7. │ └─dm::dm_sql(dm_for_sql, dest_con, table_names_out, temporary)
       8. │   └─dm::dm_ddl_post(dm, dest, table_names, temporary)
       9. │     └─dm:::ddl_get_index_defs(fks, con, table_names)
      10. │       └─... %>% ...
      11. ├─dplyr::summarize(...)
      12. ├─dplyr::group_by(., name)
      13. ├─dplyr::mutate(...)
      14. ├─dplyr:::mutate.data.frame(...)
      15. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      16. │   ├─base::withCallingHandlers(...)
      17. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      18. │     └─mask$eval_all_mutate(quo)
      19. │       └─dplyr (local) eval()
      20. ├─purrr::map_chr(...)
      21. │ └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
      22. │   └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
      23. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      24. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      25. │ └─base::stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
      26. └─base::.handleSimpleError(...)
      27.   └─dplyr (local) h(simpleError(msg, call))
      28.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Deferring nyc_comp
       Deferring dm_for_autoinc_1
       Error in `source_dir()`:
       ! Failed to evaluate './setup.R'.
       Caused by error in `local_mocked_bindings()`:
       ! Can't find binding for `tbl_sum.tbl_sql`
       Backtrace:
            ▆
         1. └─testthat::test_check("dm")
         2.   └─testthat::test_dir(...)
         3.     └─testthat:::test_files(...)
         4.       └─testthat:::test_files_serial(...)
         5.         └─testthat:::test_files_setup_state(...)
         6.           └─testthat::source_test_setup(".", env)
         7.             └─testthat::source_dir(path, "^setup.*\\.[rR]$", env = env, wrap = FALSE)
         8.               └─base::lapply(...)
         9.                 └─testthat (local) FUN(X[[i]], ...)
        10.                   └─testthat::source_file(...)
        11.                     ├─base::withCallingHandlers(...)
        12.                     └─base::eval(exprs, env)
        13.                       └─base::eval(exprs, env)
        14.                         └─testthat::local_mocked_bindings(...) at ./setup.R:14:3
        15.                           └─cli::cli_abort("Can't find binding for {.arg {missing}}")
        16.                             └─rlang::abort(...)
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

# mlr3db (0.7.1)

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
         closing unused connection 4 (<-localhost:20692)
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
       ── Error ('test-classify-diabetes.R:25:1'): (code run outside of `test_that()`) ──
       Error in `dplyr::collect(classify_diabetes(kontakter = cases_vs_nc$kontakter, diagnoser = cases_vs_nc$diagnoser, lpr_diag = cases_vs_nc$lpr_diag, lpr_adm = cases_vs_nc$lpr_adm, sysi = cases_vs_nc$sysi, sssy = cases_vs_nc$sssy, lab_forsker = cases_vs_nc$lab_forsker, bef = cases_vs_nc$bef, lmdb = cases_vs_nc$lmdb))`: Failed to collect lazy table.
       Caused by error in `DBI::dbSendQuery()`:
       ! Catalog Error: Table with name as_tbl_duckplyr_XMUENgxduz does not exist!
       Did you mean "as_tbl_duckplyr_pwmXguVtkD"?
       
       LINE 208:                     FROM as_tbl_duckplyr_XMUENgxduz
                                          ^
       i Context: rapi_prepare
       i Error type: CATALOG
       ── Error ('test-classify-variable-casing.R:9:3'): casing of input variables doesn't matter ──
       Error in `dplyr::collect(classify_diabetes(kontakter = registers$kontakter, diagnoser = registers$diagnoser, lpr_diag = registers$lpr_diag, lpr_adm = registers$lpr_adm, sysi = registers$sysi, sssy = registers$sssy, lab_forsker = registers$lab_forsker, bef = registers$bef, lmdb = registers$lmdb))`: Failed to collect lazy table.
       Caused by error in `DBI::dbSendQuery()`:
       ! Catalog Error: Table with name as_tbl_duckplyr_UkT4ROds7b does not exist!
       Did you mean "as_tbl_duckplyr_gdmBRjnkgK"?
       
       LINE 219:                     FROM as_tbl_duckplyr_UkT4ROds7b
                                          ^
       i Context: rapi_prepare
       i Error type: CATALOG
       
       [ FAIL 2 | WARN 0 | SKIP 0 | PASS 42 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# PatientProfiles (1.5.0)

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
       
       [ FAIL 2 | WARN 61 | SKIP 112 | PASS 192 ]
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

# rolap (2.5.2)

* GitHub: <https://github.com/josesamos/rolap>
* Email: <mailto:jsamos@ugr.es>
* GitHub mirror: <https://github.com/cran/rolap>

Run `revdepcheck::cloud_details(, "rolap")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       2. ├─rolap:::as_rdb.star_database(db, my_db)
       3. │ └─dm::copy_dm_to(con, db_dm, temporary = FALSE)
       4. │   └─dm::dm_sql(dm_for_sql, dest_con, table_names_out, temporary)
       5. │     └─dm::dm_ddl_post(dm, dest, table_names, temporary)
       6. │       └─dm:::ddl_get_index_defs(fks, con, table_names)
       7. │         └─... %>% ...
       8. ├─dplyr::summarize(...)
       9. ├─dplyr::group_by(., name)
      10. ├─dplyr::mutate(...)
      11. ├─dplyr:::mutate.data.frame(...)
      12. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      13. │   ├─base::withCallingHandlers(...)
      14. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      15. │     └─mask$eval_all_mutate(quo)
      16. │       └─dplyr (local) eval()
      17. ├─purrr::map_chr(...)
      18. │ └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
      19. │   └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
      20. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      21. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      22. │ └─base::stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
      23. └─base::.handleSimpleError(...)
      24.   └─dplyr (local) h(simpleError(msg, call))
      25.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         6. │       └─dm:::ddl_get_index_defs(fks, con, table_names)
         7. │         └─... %>% ...
         8. ├─dplyr::summarize(...)
         9. ├─dplyr::group_by(., name)
        10. ├─dplyr::mutate(...)
        11. ├─dplyr:::mutate.data.frame(...)
        12. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
        13. │   ├─base::withCallingHandlers(...)
        14. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
        15. │     └─mask$eval_all_mutate(quo)
        16. │       └─dplyr (local) eval()
        17. ├─purrr::map_chr(...)
        18. │ └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
        19. │   └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
        20. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
        21. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
        22. │ └─base::stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
        23. └─base::.handleSimpleError(...)
        24.   └─dplyr (local) h(simpleError(msg, call))
        25.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
       
       [ FAIL 2 | WARN 2 | SKIP 0 | PASS 326 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      14. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
      15. │     └─mask$eval_all_mutate(quo)
      16. │       └─dplyr (local) eval()
      17. ├─purrr::map_chr(...)
      18. │ └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
      19. │   └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
      20. ├─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      21. └─DBI::dbUnquoteIdentifier(con, DBI::SQL(remote_name))
      22.   └─base::stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'v60-export.Rmd' failed with diagnostics:
     ℹ In argument: `remote_name_unquoted = map_chr(...)`.
     Caused by error:
     ! Can't unquote '`mrs_cause`'
     --- failed re-building ‘v60-export.Rmd’
     
     --- re-building ‘v70-star_query.Rmd’ using rmarkdown
     --- finished re-building ‘v70-star_query.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘rolap.Rmd’ ‘v50-deploy.Rmd’ ‘v60-export.Rmd’
     
     Error: Vignette re-building failed.
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

# SCDB (0.6.0)

* GitHub: <https://github.com/ssi-dk/SCDB>
* Email: <mailto:rske@ssi.dk>
* GitHub mirror: <https://github.com/cran/SCDB>

Run `revdepcheck::cloud_details(, "SCDB")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     + ## Don't show: 
     + }) # examplesIf
     > conn <- get_connection()
     > data <- dplyr::copy_to(conn, mtcars)
     > update_snapshot(head(data, 3), conn = conn, db_table = "test.mtcars", 
     +     timestamp = "2020-01-01")
     Warning: `log_path` and `log_tbl` are both `NULL` and therefore NO file or database logging will be done.
     Consider adding options SCDB.log_table_id and/or SCDB.log_path to your .Rprofile
     2026-05-11 22:27:39.663 - root - INFO - Started
     2026-05-11 22:27:39.663 - root - INFO - Parsing data for table main.test.mtcars started
     2026-05-11 22:27:39.663 - root - INFO - Given timestamp for table is 2020-01-01
     2026-05-11 22:27:39.928 - root - INFO - Calculated checksums
     2026-05-11 22:27:40.053 - root - INFO - Deactivating records
     Warning: `as.sql()` was deprecated in dbplyr 2.6.0.
     ℹ Please use `as_table_path()` instead.
     ℹ The deprecated feature was likely used in the SCDB package.
       Please report the issue at <https://github.com/ssi-dk/SCDB/issues>.
     Warning: `table` uses SQL where a table identifier is expected.
     ℹ If you want to use a literal (unquoted) identifier use `I()` instead.
     2026-05-11 22:27:40.234 - root - INFO - Deactivate records count: 0
     2026-05-11 22:27:40.235 - root - INFO - Adding new records
     Warning: `table` uses SQL where a table identifier is expected.
     ℹ If you want to use a literal (unquoted) identifier use `I()` instead.
     Error: no such table: `SCDB_digest_to_checksum_Ry23f5MYPf`
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Actually got a <simpleError> with message:
         no such table: `SCDB_digest_to_checksum_owSiB2vsWr`
       ── Failure ('test-update_snapshot.R:624:5'): update_snapshot() works with across connection ──
       Expected `update_snapshot(...)` not to throw any errors.
       Actually got a <simpleError> with message:
         no such table: `SCDB_digest_to_checksum_0vK31SlsIG`
       ── Failure ('test-update_snapshot.R:670:5'): update_snapshot() works with across connection ──
       Expected `update_snapshot(...)` not to throw any errors.
       Actually got a <simpleError> with message:
         no such table: `SCDB_digest_to_checksum_ba66XgC5LW`
       ── Failure ('test-update_snapshot.R:670:5'): update_snapshot() works with across connection ──
       Expected `update_snapshot(...)` not to throw any errors.
       Actually got a <simpleError> with message:
         no such table: `SCDB_digest_to_checksum_psNiqJuMng`
       
       [ FAIL 22 | WARN 82 | SKIP 1 | PASS 633 ]
       Error:
       ! Test failures.
       Warning messages:
       1: `as.sql()` was deprecated in dbplyr 2.6.0.
       i Please use `as_table_path()` instead. 
       2: `as.sql()` was deprecated in dbplyr 2.6.0.
       i Please use `as_table_path()` instead. 
       3: call dbDisconnect() when finished working with a connection 
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ℹ Error type: BINDER
     ---
     Backtrace:
         ▆
      1. └─global update_snapshot(...)
      2.   └─SCDB::update_snapshot(logger = SCDB::LoggerNull$new(), ...)
      3.     ├─DBI::dbSendQuery(conn, sql_insert)
      4.     └─DBI::dbSendQuery(conn, sql_insert)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'delta-loading.Rmd' failed with diagnostics:
     Binder Error: Cannot extract field 'checksum' from expression "'dbplyr_tmp_GBGkuYOFYx'" because it is not a struct, union, map, or json
     ℹ Context: rapi_prepare
     ℹ Error type: BINDER
     --- failed re-building ‘delta-loading.Rmd’
     
     --- re-building ‘slowly-changing-dimension.Rmd’ using rmarkdown
     Loading required namespace: tidyverse
     --- finished re-building ‘slowly-changing-dimension.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘SCDB.Rmd’ ‘delta-loading.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# when (1.0.0)

* GitHub: <https://github.com/josesamos/when>
* Email: <mailto:jsamos@ugr.es>
* GitHub mirror: <https://github.com/cran/when>

Run `revdepcheck::cloud_details(, "when")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-export.R:43:3'): export ──────────────────────────────────────
       Expected `tables` to equal "date".
       Differences:
       `actual`:   "`date`"
       `expected`: "date"  
       
       ── Failure ('test-vignette_when.R:546:3'): vignette_when ───────────────────────
       Expected `n14` to equal "dim_where".
       Differences:
       `actual`:   "`dim_where`"
       `expected`: "dim_where"  
       
       ── Failure ('test-vignette_when.R:548:3'): vignette_when ───────────────────────
       Expected `n15` to equal "dim_where".
       Differences:
       `actual`:   "`dim_where`"
       `expected`: "dim_where"  
       
       
       [ FAIL 3 | WARN 0 | SKIP 0 | PASS 148 ]
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
         ‘define_characteristics.Rd’ ‘define_instances.Rd’ ‘export.R’
         ‘generate_table.Rd’ ‘get_attribute_definition_function.Rd’
         ‘get_day_part.Rd’ ‘get_level_attribute_names.Rd’ ‘get_level_names.Rd’
         ‘get_table.Rd’ ‘get_table_attribute_names.Rd’ ‘get_table_csv.Rd’
         ‘get_table_rdb.Rd’ ‘get_table_xlsx.Rd’ ‘select_date_levels.Rd’
         ‘select_day_level.Rd’ ‘select_month_level.Rd’
         ‘select_quarter_level.Rd’ ‘select_semester_level.Rd’
         ‘select_time_level.Rd’ ‘select_week_level.Rd’ ‘select_year_level.Rd’
         ‘set_attribute_definition_function.Rd’ ‘set_day_part.Rd’
         ‘set_table_attribute_names.Rd’ ‘week.R’
     ```

