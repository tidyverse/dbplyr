# cytominer

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/cytominer
* URL: https://github.com/cytomining/cytominer
* BugReports: https://github.com/cytomining/cytominer/issues
* Date/Publication: 2017-09-17 18:25:04 UTC
* Number of recursive dependencies: 91

Run `revdep_details(,"cytominer")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: dbSendQuery(con, sql) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/verb-compute.R:119
      17: dbSendQuery(con, sql)
      18: .local(conn, statement, ...)
      19: new("SQLiteResult", sql = statement, ptr = result_create(conn@ptr, statement), conn = conn, 
             bigint = conn@bigint)
      20: initialize(value, ...)
      21: initialize(value, ...)
      22: result_create(conn@ptr, statement)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 41 SKIPPED: 0 FAILED: 1
      1. Error: `aggregate` aggregates data (@test-aggregate.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dplyr.teradata

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/dplyr.teradata
* Date/Publication: 2019-03-20 21:30:02 UTC
* Number of recursive dependencies: 53

Run `revdep_details(,"dplyr.teradata")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             }
         }) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/translate-sql.R:146
      8: FUN(X[[i]], ...)
      9: escape(overscope_eval_next(overscope, x), con = con) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/translate-sql.R:154
      10: overscope_eval_next(overscope, x) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/translate-sql.R:154
      11: case_when(x == 1L ~ 1L, x == 2L ~ 2L, TRUE ~ 3L)
      12: translate_sql_(list(f[[2]])) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/dplyr.teradata/new/dplyr.teradata.Rcheck/00_pkg_src/dplyr.teradata/R/translate-sql-base-teradata.R:18
      13: sql_translate_env(con) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/translate-sql.R:145
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: custom scalar translated correctly (@test-translate-teradata.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bit64’ ‘rstudioapi’
      All declared Imports should be used.
    ```

# MonetDBLite

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/MonetDBLite
* URL: https://github.com/hannesmuehleisen/MonetDBLite-R
* BugReports: https://github.com/hannesmuehleisen/MonetDBLite-R/issues
* Date/Publication: 2018-07-27 09:40:03 UTC
* Number of recursive dependencies: 72

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
      9: c_character(...) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/sql.R:11
      10: dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", as.integer(size)) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/new/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      11: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/build-sql.R:34
      
      [1] "E"
      [1] "H"
      [1] "M"
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 308 SKIPPED: 13 FAILED: 1
      1. Error: sample works (@test_04_dplyr.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# Organism.dplyr

<details>

* Version: 1.10.0
* Source code: ???
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 105

Run `revdep_details(,"Organism.dplyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [32] 95456 - 95461 == -5
      [33] 95456 - 95461 == -5
      [34] 95456 - 95461 == -5
      [35] 95456 - 95461 == -5
      [47] 95461 - 95456 ==  5
      ...
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 201 SKIPPED: 0 FAILED: 1
      1. Failure: select (@test-src_organism-select.R#44) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .toGRanges: no visible binding for global variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractors.R:236)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:254-255)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:264-265)
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434-435)
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:436)
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:436)
    Undefined global functions or variables:
      . OrgDb name organism
    ```

# pivot

<details>

* Version: 18.4.17
* Source code: https://github.com/cran/pivot
* Date/Publication: 2018-05-01 09:49:18 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"pivot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > # establish db as a database connection
    > ## Don't show: 
    >    con <- simulate_mssql()
    >    src <- src_dbi(con)
    >    base <- list( x = ident('##iris')
    +                , vars  = tbl_vars(iris)
    +                ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))
    >    db_iris <- structure( list( src = src
    +                              , ops = base
    +                              )
    +        , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))
    > ## End(Don't show)
    > ## Not run: 
    > ##D db_iris <- copy_to(db, iris)
    > ## End(Not run)
    > result <- pivot( db_iris, Species, mean(Petal.Length, na.rm=TRUE)
    +                , setosa, versicolor, virginica)
    > sql_render(result)
    Error: `con` must not be NULL
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      25: .abort(text)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 65 SKIPPED: 1 FAILED: 8
      1. Error: PIVOT construction (@test-pivot.R#44) 
      2. Error: PIVOT warnings and errors (@test-pivot.R#71) 
      3. Error: spread.tbl_lazy (@test-tidyr.R#8) 
      4. Failure: UNPIVOT construction (@test-unpivot.R#45) 
      5. Failure: order_by (@test-unpivot.R#81) 
      6. Failure: find_connection (@test-utils.R#11) 
      7. Failure: find_connection (@test-utils.R#12) 
      8. Error: get_pivot_levels (@test-utils.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘odbc’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘lubridate’
      All declared Imports should be used.
    ```

# RPresto

<details>

* Version: 1.3.2
* Source code: https://github.com/cran/RPresto
* URL: https://github.com/prestodb/RPresto
* BugReports: https://github.com/prestodb/RPresto/issues
* Date/Publication: 2018-10-23 04:10:02 UTC
* Number of recursive dependencies: 32

Run `revdep_details(,"RPresto")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpTYek75/R.INSTALLf635a6ce243/dbplyr/R/build-sql.R:34
      
      [31m--[39m [31m2. Failure: as() works (@test-translate_sql.R#26) [39m [31m-----------------------------------[39m
      translate_sql(pmax(x), con = s[["con"]]) not equal to dplyr::sql("GREATEST(\"x\")").
      1/1 mismatches
      x[1]: "greatest(\"x\")"
      y[1]: "GREATEST(\"x\")"
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════
      OK: 278 SKIPPED: 28 FAILED: 2
      1. Error: db_explain works with mock (@test-db_explain.R#26) 
      2. Failure: as() works (@test-translate_sql.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

