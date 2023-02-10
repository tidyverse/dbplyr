# dcmodifydb

<details>

* Version: 0.3.1
* GitHub: https://github.com/data-cleaning/dcmodifydb
* Source code: https://github.com/cran/dcmodifydb
* Date/Publication: 2022-06-17 15:10:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "dcmodifydb")` for more info

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
     23.                           └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │       └─purrr::map_chr(enexprs(...), escape_expr, con = con)
        6. │         └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
        7. │           ├─purrr:::with_indexed_errors(...)
        8. │           │ └─base::withCallingHandlers(...)
        9. │           ├─purrr:::call_with_cleanup(...)
       10. │           └─dbplyr (local) .f(.x[[i]], ...)
       11. │             └─dbplyr::escape(val, con = con)
       12. └─base::.handleSimpleError(...)
       13.   └─purrr (local) h(simpleError(msg, call))
       14.     └─cli::cli_abort(...)
       15.       └─rlang::abort(...)
      
      [ FAIL 3 | WARN 0 | SKIP 5 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# validatedb

<details>

* Version: 0.1.4
* GitHub: https://github.com/data-cleaning/validatedb
* Source code: https://github.com/cran/validatedb
* Date/Publication: 2021-10-06 10:20:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "validatedb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ! Object `y` not found.
      ── Error ('test-summary.R:32'): summary: works with failing rules ──────────────
      Error in `dplyr::transmute(tbl, row = row_number(), rule = "V5", fail = !y > 
          0)`: ℹ In argument: `fail = !y > 0`
      Caused by error:
      ! Object `y` not found.
      ── Error ('test-summary.R:73'): summary: works with failing rules (sparse) ─────
      Error in `dplyr::transmute(tbl, row = row_number(), rule = "V5", fail = !y > 
          0)`: ℹ In argument: `fail = !y > 0`
      Caused by error:
      ! Object `y` not found.
      
      [ FAIL 3 | WARN 0 | SKIP 2 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

