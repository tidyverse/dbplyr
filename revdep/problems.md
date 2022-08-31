# bcdata

<details>

* Version: 0.3.2
* GitHub: https://github.com/bcgov/bcdata
* Source code: https://github.com/cran/bcdata
* Date/Publication: 2022-07-06 07:30:02 UTC
* Number of recursive dependencies: 120

Run `revdep_details(, "bcdata")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.   └─base::lapply(...)
        6.     └─bcdata (local) FUN(X[[i]], ...)
        7.       ├─rlang::new_quosure(...)
        8.       └─dbplyr::partial_eval(x, data = dbplyr::lazy_frame())
        9.         └─dbplyr::partial_eval(get_expr(call), data, get_env(call), error_call = error_call)
       10.           └─dbplyr:::partial_eval_call(call, data, env)
       11.             └─base::lapply(call[-1], partial_eval, data = data, env = env)
       12.               └─dbplyr (local) FUN(X[[i]], ...)
       13.                 └─dbplyr:::partial_eval_sym(call, data, env)
       14.                   └─cli::cli_abort("Object {.var {name}} not found.", call = NULL)
       15.                     └─rlang::abort(message, ..., call = call, use_cli_format = TRUE)
      
      [ FAIL 3 | WARN 0 | SKIP 101 | PASS 63 ]
      Error: Test failures
      Execution halted
    ```

# dcmodifydb

<details>

* Version: 0.3.1
* GitHub: https://github.com/data-cleaning/dcmodifydb
* Source code: https://github.com/cran/dcmodifydb
* Date/Publication: 2022-06-17 15:10:02 UTC
* Number of recursive dependencies: 73

Run `revdep_details(, "dcmodifydb")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dcmodifydb-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: is_working_db
    > ### Title: Rule check on the database
    > ### Aliases: is_working_db
    > 
    > ### ** Examples
    > 
    > person <- dbplyr::memdb_frame(age = 12, salary = 3000)
    ...
     15.               │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     16.               │ └─base::withCallingHandlers(...)
     17.               └─dbplyr::partial_eval(get_expr(x), data, get_env(x), error_call = error_call)
     18.                 └─dbplyr:::partial_eval_call(call, data, env)
     19.                   └─base::lapply(call[-1], partial_eval, data = data, env = env)
     20.                     └─dbplyr (local) FUN(X[[i]], ...)
     21.                       └─dbplyr:::partial_eval_sym(call, data, env)
     22.                         └─cli::cli_abort("Object {.var {name}} not found.", call = NULL)
     23.                           └─rlang::abort(message, ..., call = call, use_cli_format = TRUE)
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-is_working.R:27:5): is working: is working check ────────────────
      Error in `dplyr::filter(table, z > 2)`: Problem while computing `..1 = z > 2`
      Caused by error:
      ! Object `z` not found.
      ── Error (test-modify.R:96:5): modify: handles a non-working rule ──────────────
      Error in `dplyr::filter(table, y > 1)`: Problem while computing `..1 = y > 1`
      Caused by error:
      ! Object `y` not found.
      
      [ FAIL 2 | WARN 0 | SKIP 5 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# tidyhydat

<details>

* Version: 0.5.6
* GitHub: https://github.com/ropensci/tidyhydat
* Source code: https://github.com/cran/tidyhydat
* Date/Publication: 2022-08-20 07:10:02 UTC
* Number of recursive dependencies: 95

Run `revdep_details(, "tidyhydat")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `dplyr::filter(sed_monthly_loads, !!sym_YEAR >= lubridate::year(start_date))`: Problem while computing `..1 = Year >= lubridate::year(start_date)`
      Caused by error:
      ! Object `Year` not found.
      ── Error (test_hy_sed_monthly_suscon.R:51:3): hy_sed_monthly_suscon respects Date specification ──
      Error in `dplyr::filter(sed_monthly_suscon, !!sym_YEAR >= lubridate::year(start_date))`: Problem while computing `..1 = Year >= lubridate::year(start_date)`
      Caused by error:
      ! Object `Year` not found.
      ── Error (test_hy_sed_monthly_suscon.R:64:3): functions that accept a date argument return data when specifying only the start date or end date ──
      Error in `dplyr::filter(sed_monthly_suscon, !!sym_YEAR >= lubridate::year(start_date))`: Problem while computing `..1 = Year >= lubridate::year(start_date)`
      Caused by error:
      ! Object `Year` not found.
      
      [ FAIL 8 | WARN 0 | SKIP 18 | PASS 187 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        test_db   3.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 122 marked UTF-8 strings
    ```

# validatedb

<details>

* Version: 0.1.4
* GitHub: https://github.com/data-cleaning/validatedb
* Source code: https://github.com/cran/validatedb
* Date/Publication: 2021-10-06 10:20:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(, "validatedb")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ! Object `y` not found.
      ── Error (test-summary.R:32:5): summary: works with failing rules ──────────────
      Error in `dplyr::transmute(tbl, row = row_number(), rule = "V5", fail = !y > 
          0)`: Problem while computing `fail = !y > 0`
      Caused by error:
      ! Object `y` not found.
      ── Error (test-summary.R:73:5): summary: works with failing rules (sparse) ─────
      Error in `dplyr::transmute(tbl, row = row_number(), rule = "V5", fail = !y > 
          0)`: Problem while computing `fail = !y > 0`
      Caused by error:
      ! Object `y` not found.
      
      [ FAIL 3 | WARN 0 | SKIP 2 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

