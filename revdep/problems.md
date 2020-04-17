# arkdb

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/arkdb
* URL: https://github.com/ropensci/arkdb
* BugReports: https://github.com/ropensci/arkdb/issues
* Date/Publication: 2018-10-31 21:10:03 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"arkdb")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("arkdb")
      [31m──[39m [31m1. Error: (unknown) (@test-arkdb.R#172) [39m [31m───────────────────────────────────────[39m
      $ operator not defined for this S4 class
      [1mBacktrace:[22m
      [90m 1. [39mDBI::dbDisconnect(db$con)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════
      [ OK: 29 | SKIPPED: 1 | WARNINGS: 5 | FAILED: 1 ]
      1. Error: (unknown) (@test-arkdb.R#172) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In connection_release(conn@ptr) : Already disconnected
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘MonetDBLite’
    ```

# dm

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/dm
* Date/Publication: 2020-03-12 17:30:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"dm")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Evaluating t1_src
      Evaluating t3_src
      [31m──[39m [31m1. Failure: check_cardinality_...() functions are checking the cardinality corre[39m
      `map2(...)` produced warnings.
      
      <unknown>:1753848: Invalid asm.js: Function definition doesn't match use
      ══ testthat results  ══════════════════════════════════════════════════════════════
      [ OK: 739 | SKIPPED: 42 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: check_cardinality_...() functions are checking the cardinality correctly? (@test-check-cardinalities.R#7) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In file(filename, "r", encoding = encoding) :
        cannot open file '/Users/tobiasschieferdecker/git/cynkra/dm/.Rprofile': No such file or directory
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DT’
      All declared Imports should be used.
    ```

# resourcer

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/resourcer
* BugReports: https://github.com/obiba/resourcer
* Date/Publication: 2020-04-02 15:50:02 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"resourcer")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

