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
        60. │                                   ├─data.table::setDT(...)
        61. │                                   │ └─data.table::is.data.table(x)
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
       Warning message:
       call dbDisconnect() when finished working with a connection 
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

# SCDB (0.6.1)

* GitHub: <https://github.com/ssi-dk/SCDB>
* Email: <mailto:rske@ssi.dk>
* GitHub mirror: <https://github.com/cran/SCDB>

Run `revdepcheck::cloud_details(, "SCDB")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • empty test (1):
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-getTableSignature.R:364:7'): getTableSignature() generates consistent data types (SQLiteConnection) ──
       Expected `dplyr::collect(remote_data_2)` to be identical to `dplyr::collect(remote_data_1)`.
       Differences:
       `actual$Date` is a double vector (20591)
       `expected$Date` is an integer vector (20591)
       
       ── Failure ('test-getTableSignature.R:364:7'): getTableSignature() generates consistent data types (SQLiteConnection) ──
       Expected `dplyr::collect(remote_data_2)` to be identical to `dplyr::collect(remote_data_1)`.
       Differences:
       `actual$Date` is a double vector (20591)
       `expected$Date` is an integer vector (20591)
       
       
       [ FAIL 2 | WARN 310 | SKIP 1 | PASS 782 ]
       Error:
       ! Test failures.
       Warning messages:
       1: `as.sql()` was deprecated in dbplyr 2.6.0.
       i Please use `as_table_path()` instead. 
       2: `as.sql()` was deprecated in dbplyr 2.6.0.
       i Please use `as_table_path()` instead. 
       Execution halted
     ```

