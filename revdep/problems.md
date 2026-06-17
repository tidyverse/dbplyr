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

