# Andromeda

<details>

* Version: 0.6.0
* GitHub: https://github.com/OHDSI/Andromeda
* Source code: https://github.com/cran/Andromeda
* Date/Publication: 2022-01-25 12:12:51 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "Andromeda")` for more info

</details>

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
    ...
    > # [1] 50
    > 
    > appendToTable(andr$cars, cars)
    Error in `appendToTable()`:
    ! First argument must be a base table (cannot be a query result)
    Backtrace:
        ▆
     1. └─Andromeda::appendToTable(andr$cars, cars)
     2.   └─rlang::abort("First argument must be a base table (cannot be a query result)")
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error (test-batching.R:30:3): batchApply safe mode ──────────────────────────
      Error in `appendToTable(andromeda$cars2, batch)`: First argument must be a base table (cannot be a query result)
      Backtrace:
          ▆
       1. └─Andromeda::batchApply(...) at test-batching.R:30:2
       2.   ├─base::tryCatch(...)
       3.   │ └─base tryCatchList(expr, classes, parentenv, handlers)
       4.   ├─base::do.call(fun, append(list(batch), list(...)))
       5.   └─Andromeda `<fn>`(`<df[,2]>`, multiplier = 2)
       6.     └─Andromeda::appendToTable(andromeda$cars2, batch) at test-batching.R:27:6
       7.       └─rlang::abort("First argument must be a base table (cannot be a query result)")
      
      [ FAIL 7 | WARN 0 | SKIP 1 | PASS 85 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘UsingAndromeda.Rmd’ using rmarkdown
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    ...
    Quitting from lines 58-59 (UsingAndromeda.Rmd) 
    Error: processing vignette 'UsingAndromeda.Rmd' failed with diagnostics:
    First argument must be a base table (cannot be a query result)
    --- failed re-building ‘UsingAndromeda.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘UsingAndromeda.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# apache.sedona

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/apache.sedona
* Date/Publication: 2021-11-23 19:40:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "apache.sedona")` for more info

</details>

## Newly broken

*   checking whether package ‘apache.sedona’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/apache.sedona/new/apache.sedona.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘dplyr’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘apache.sedona’ ...
** package ‘apache.sedona’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘add_op_single’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘apache.sedona’
* removing ‘/tmp/workdir/apache.sedona/new/apache.sedona.Rcheck/apache.sedona’


```
### CRAN

```
* installing *source* package ‘apache.sedona’ ...
** package ‘apache.sedona’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (apache.sedona)


```
# bcdata

<details>

* Version: 0.3.0
* GitHub: https://github.com/bcgov/bcdata
* Source code: https://github.com/cran/bcdata
* Date/Publication: 2021-10-28 17:00:06 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "bcdata")` for more info

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
       1. ├─testthat::expect_equal(...) at test-query-geodata-filter.R:121:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─bcdata:::cql_translate(foo == x[1])
       5.   └─base::lapply(...)
       6.     └─bcdata FUN(X[[i]], ...)
       7.       ├─rlang::new_quosure(dbplyr::partial_eval(x), rlang::get_env(x))
       8.       └─dbplyr::partial_eval(x)
      
      [ FAIL 3 | WARN 0 | SKIP 96 | PASS 63 ]
      Error: Test failures
      Execution halted
    ```

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

# geospark

<details>

* Version: 0.3.1
* GitHub: https://github.com/harryprince/geospark
* Source code: https://github.com/cran/geospark
* Date/Publication: 2020-03-02 05:40:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "geospark")` for more info

</details>

## Newly broken

*   checking whether package ‘geospark’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/geospark/new/geospark.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘geospark’ ...
** package ‘geospark’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘add_op_single’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘geospark’
* removing ‘/tmp/workdir/geospark/new/geospark.Rcheck/geospark’


```
### CRAN

```
* installing *source* package ‘geospark’ ...
** package ‘geospark’ successfully unpacked and MD5 sums checked
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
* DONE (geospark)


```
# sparklyr

<details>

* Version: 1.7.5
* GitHub: https://github.com/sparklyr/sparklyr
* Source code: https://github.com/cran/sparklyr
* Date/Publication: 2022-02-02 14:30:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "sparklyr")` for more info

</details>

## Newly broken

*   checking whether package ‘sparklyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparklyr/new/sparklyr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        java   3.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lifecycle’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘sparklyr’ ...
** package ‘sparklyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘add_op_single’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparklyr’
* removing ‘/tmp/workdir/sparklyr/new/sparklyr.Rcheck/sparklyr’


```
### CRAN

```
* installing *source* package ‘sparklyr’ ...
** package ‘sparklyr’ successfully unpacked and MD5 sums checked
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
* DONE (sparklyr)


```
# sparklyr.flint

<details>

* Version: 0.2.2
* GitHub: https://github.com/r-spark/sparklyr.flint
* Source code: https://github.com/cran/sparklyr.flint
* Date/Publication: 2022-01-11 08:50:13 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "sparklyr.flint")` for more info

</details>

## Newly broken

*   checking whether package ‘sparklyr.flint’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparklyr.flint/new/sparklyr.flint.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sparklyr.flint’ ...
** package ‘sparklyr.flint’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘add_op_single’ is not exported by 'namespace:dbplyr'
Execution halted
ERROR: lazy loading failed for package ‘sparklyr.flint’
* removing ‘/tmp/workdir/sparklyr.flint/new/sparklyr.flint.Rcheck/sparklyr.flint’


```
### CRAN

```
* installing *source* package ‘sparklyr.flint’ ...
** package ‘sparklyr.flint’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sparklyr.flint)


```
# taxadb

<details>

* Version: 0.1.4
* GitHub: https://github.com/ropensci/taxadb
* Source code: https://github.com/cran/taxadb
* Date/Publication: 2022-03-05 15:40:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "taxadb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5.       ├─dbplyr::db_sql_render(x$src$con, x)
        6.       └─dbplyr:::db_sql_render.DBIConnection(x$src$con, x)
        7.         ├─dbplyr::sql_render(sql, con = con, ..., cte = cte)
        8.         └─dbplyr:::sql_render.tbl_lazy(sql, con = con, ..., cte = cte)
        9.           ├─dbplyr::sql_render(...)
       10.           └─dbplyr:::sql_render.lazy_query(...)
       11.             ├─dbplyr::sql_render(...)
       12.             └─dbplyr:::sql_render.set_op_query(...)
       13.               └─dbplyr:::dbplyr_query_set_op(...)
       14.                 └─dbplyr:::dbplyr_fallback(con, "sql_set_op", ...)
       15.                   └─rlang::eval_bare(expr((!!fun)(con, ...)))
      
      [ FAIL 2 | WARN 1 | SKIP 4 | PASS 59 ]
      Error: Test failures
      Execution halted
    ```

# tidypredict

<details>

* Version: 0.4.8
* GitHub: https://github.com/tidymodels/tidypredict
* Source code: https://github.com/cran/tidypredict
* Date/Publication: 2020-10-28 06:50:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "tidypredict")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1/1 mismatches
      x[1]: "\"((((0.0 + CASE\\nWHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`
      x[1]: wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545\\nWHEN (`qsec` >= 19
      x[1]: .9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009\\nWH
      x[1]: EN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.14166
      x[1]: 6666\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003\\nE...
      y[1]: "\"0.0 + CASE\\nWHEN ((`qsec` < 19.9549999 OR ((`qsec`) IS NULL)) AND (`wt
      y[1]: ` < 3.18000007 OR ((`wt`) IS NULL))) THEN (0.138461545)\\nWHEN (`qsec` >= 
      y[1]: 19.9549999 AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (-0.100000009
      y[1]: )\\nWHEN ((`hp` < 290.0 OR ((`hp`) IS NULL)) AND `wt` >= 3.18000007) THEN 
      y[1]: (-0.141666666)\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN (0.07...
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 117 ]
      Error: Test failures
      Execution halted
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

