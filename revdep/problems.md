# MonetDBLite

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/MonetDBLite
* URL: https://github.com/hannesmuehleisen/MonetDBLite-R
* BugReports: https://github.com/hannesmuehleisen/MonetDBLite-R/issues
* Date/Publication: 2018-07-27 09:40:03 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"MonetDBLite")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: dbplyr::sql(dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", 
             as.integer(size))) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/new/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      9: c_character(...) at /private/tmp/RtmpuqofJs/R.INSTALL38a48127a87/dbplyr/R/sql.R:11
      10: dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", as.integer(size)) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/new/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      11: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpuqofJs/R.INSTALL38a48127a87/dbplyr/R/build-sql.R:34
      
      [1] "E"
      [1] "H"
      [1] "M"
      ══ testthat results  ════════════════════════════════════════════════════════════════════════
      OK: 297 SKIPPED: 13 WARNINGS: 11 FAILED: 1
      1. Error: sample works (@test_04_dplyr.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

