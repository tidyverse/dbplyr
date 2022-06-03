# dcmodifydb

<details>

* Version: 0.2.0
* GitHub: https://github.com/data-cleaning/dcmodifydb
* Source code: https://github.com/cran/dcmodifydb
* Date/Publication: 2022-01-21 12:42:41 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "dcmodifydb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(dcmodifydb)
      > 
      > test_check("dcmodifydb")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 33 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-update.R:44:5): update: generates update statements ───────────
      sql[[1]] not equal to sql("UPDATE `mtcars`\nSET `gear` = 0\nWHERE ((`gear`) IS NULL);").
      1/1 mismatches
      x[1]: "UPDATE `mtcars`\nSET `gear` = 0\nWHERE (`gear` IS NULL);"
      y[1]: "UPDATE `mtcars`\nSET `gear` = 0\nWHERE ((`gear`) IS NULL);"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# dittodb

<details>

* Version: 0.1.3
* GitHub: https://github.com/ropensci/dittodb
* Source code: https://github.com/cran/dittodb
* Date/Publication: 2020-10-10 06:20:02 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "dittodb")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘developing-dittodb.Rmd’ using rmarkdown
    --- finished re-building ‘developing-dittodb.Rmd’
    
    --- re-building ‘dittodb.Rmd’ using rmarkdown
    --- finished re-building ‘dittodb.Rmd’
    
    --- re-building ‘nycflights.Rmd’ using rmarkdown
    --- finished re-building ‘nycflights.Rmd’
    
    ...
    Quitting from lines 166-191 (travelling.Rmd) 
    Error: processing vignette 'travelling.Rmd' failed with diagnostics:
    Couldn't find the file travelling/SELECT-a92ba3.R in any of the mock directories.
    --- failed re-building ‘travelling.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘travelling.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# dlookr

<details>

* Version: 0.5.6
* GitHub: https://github.com/choonghyunryu/dlookr
* Source code: https://github.com/cran/dlookr
* Date/Publication: 2022-04-11 15:12:29 UTC
* Number of recursive dependencies: 170

Run `cloud_details(, "dlookr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘EDA.Rmd’ using rmarkdown
    
    Attaching package: 'dlookr'
    
    The following object is masked from 'package:base':
    
        transform
    
    
    ...
    
        intersect, setdiff, setequal, union
    
    --- finished re-building ‘transformation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EDA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dplyr.teradata

<details>

* Version: 0.4.1
* GitHub: https://github.com/hoxo-m/dplyr.teradata
* Source code: https://github.com/cran/dplyr.teradata
* Date/Publication: 2020-11-12 11:30:06 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "dplyr.teradata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: odbc
      > 
      > test_check("dplyr.teradata")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 7 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-translate-teradata.R:9:3): custom scalar translated correctly ──
      trans(case_when(x == 1L ~ 1L, x == 2L ~ 2L, TRUE ~ 3L)) not equal to sql("CASE\nWHEN (`x` = 1) THEN (1)\nWHEN (`x` = 2) THEN (2)\nELSE (3)\nEND").
      1/1 mismatches
      x[1]: "CASE WHEN (`x` = 1) THEN 1 WHEN (`x` = 2) THEN 2 ELSE 3 END"
      y[1]: "CASE\nWHEN (`x` = 1) THEN (1)\nWHEN (`x` = 2) THEN (2)\nELSE (3)\nEND"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    sample_n.tbl_Teradata: no visible global function definition for
      ‘op_single’
    Undefined global functions or variables:
      op_single
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# RClickhouse

<details>

* Version: 0.6.3
* GitHub: https://github.com/IMSMWU/RClickhouse
* Source code: https://github.com/cran/RClickhouse
* Date/Publication: 2022-03-08 13:40:08 UTC
* Number of recursive dependencies: 42

Run `cloud_details(, "RClickhouse")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.5Mb
      sub-directories of 1Mb or more:
        libs  18.2Mb
    ```

# sparklyr

<details>

* Version: 1.7.6
* GitHub: https://github.com/sparklyr/sparklyr
* Source code: https://github.com/cran/sparklyr
* Date/Publication: 2022-05-26 15:20:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "sparklyr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘dbplyr::add_op_single’ ‘dbplyr::op_single’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      1.6Mb
        java   3.4Mb
    ```

# VicmapR

<details>

* Version: 0.1.8
* GitHub: https://github.com/justincally/VicmapR
* Source code: https://github.com/cran/VicmapR
* Date/Publication: 2021-11-10 07:50:19 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "VicmapR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `dbplyr::partial_eval(x)`: argument "data" is missing, with no default
      Backtrace:
          ▆
       1. ├─testthat::expect_is(...) at test-cql-predicates.R:90:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─VicmapR:::cql_translate(CQL(INTERSECTS(the_geom)))
       5.   └─base::lapply(...)
       6.     └─VicmapR FUN(X[[i]], ...)
       7.       ├─rlang::new_quosure(dbplyr::partial_eval(x), rlang::get_env(x))
       8.       └─dbplyr::partial_eval(x)
      
      [ FAIL 1 | WARN 0 | SKIP 12 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

