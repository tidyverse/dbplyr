# arkdb

<details>

* Version: 0.0.6
* GitHub: https://github.com/ropensci/arkdb
* Source code: https://github.com/cran/arkdb
* Date/Publication: 2020-09-18 05:30:03 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "arkdb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("arkdb")
      [1] "Testing using backend duckdb_connection"
      ── 1. Error: (unknown) (@test-arkdb.R#167)  ────────────────────────────────────
      error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': $ operator not defined for this S4 class
      Backtrace:
       1. DBI::dbDisconnect(db$con)
       2. base::.handleSimpleError(...)
       3. base:::h(simpleError(msg, call))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 32 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-arkdb.R#167) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bcdata

<details>

* Version: 0.2.0
* GitHub: https://github.com/bcgov/bcdata
* Source code: https://github.com/cran/bcdata
* Date/Publication: 2020-06-25 07:00:02 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "bcdata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. dbplyr:::FUN(X[[i]], ...)
        8. dbplyr:::sql_data_mask(x, variant, con = con, window = window)
        9. dbplyr:::ceply(idents, escape, con = con, parent = special_calls2)
       10. base::lapply(x, f, ...)
       12. dbplyr:::escape.ident(X[[i]], ...)
       14. dbplyr:::sql_escape_ident.DBIConnection(con, x)
       15. DBI::dbQuoteIdentifier(con, x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 65 | SKIPPED: 82 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: unsupported aggregation functions fail correctly (@test-cql-string.R#87) 
      2. Error: subsetting works locally (@test-query-geodata-filter.R#122) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# chunked

<details>

* Version: 0.5.0
* GitHub: https://github.com/edwindj/chunked
* Source code: https://github.com/cran/chunked
* Date/Publication: 2020-03-24 08:20:02 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "chunked")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. chunked::write_chunkwise(tbl_iris, db, "iris")
       2. chunked:::write_chunkwise.chunkwise(tbl_iris, db, "iris")
       3. chunked::insert_chunkwise_into(x, dest, table, ...)
       4. dplyr::db_begin(con)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 42 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: insert_chunkwise_into: can insert into a db (@test-insert-into.R#9) 
      2. Error: insert_chunkwise_into: can insert modified table into a db (@test-insert-into.R#18) 
      3. Error: write_chunkwise to db works (@test-write.R#26) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# dittodb

<details>

* Version: 0.1.1
* GitHub: https://github.com/ropensci/dittodb
* Source code: https://github.com/cran/dittodb
* Date/Publication: 2020-07-29 12:00:15 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "dittodb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("dittodb")
      Loading required package: dittodb
      Loading required package: DBI
      ── 1. Failure: (unknown) (@test-dbplyr-integration.R#59)  ──────────────────────
      `flights_db <- tbl(con, "flights")` did not produce any warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 160 | SKIPPED: 7 | WARNINGS: 2 | FAILED: 1 ]
      1. Failure: (unknown) (@test-dbplyr-integration.R#59) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# mdsr

<details>

* Version: 0.2.0
* GitHub: https://github.com/beanumber/mdsr
* Source code: https://github.com/cran/mdsr
* Date/Publication: 2020-09-03 22:32:09 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "mdsr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > 
      > test_check("mdsr")
      ── 1. Error: scidb works (@tests.R#18)  ────────────────────────────────────────
      no applicable method for 'db_list_tables' applied to an object of class "c('MySQLConnection', 'DBIConnection', 'DBIObject')"
      Backtrace:
       1. testthat::expect_length(dplyr::db_list_tables(x$con), 4)
       4. dplyr::db_list_tables(x$con)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 5 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: scidb works (@tests.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   6.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2866 marked UTF-8 strings
    ```

# parsemsf

<details>

* Version: 0.1.1
* GitHub: https://github.com/benjaminjack/parsemsf
* Source code: https://github.com/cran/parsemsf
* Date/Publication: 2017-12-09 22:00:10 UTC
* Number of recursive dependencies: 75

Run `cloud_details(, "parsemsf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘parsemsf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_area_table
    > ### Title: Make a table of peptide areas
    > ### Aliases: make_area_table
    > 
    > ### ** Examples
    > 
    > make_area_table(parsemsf_example("test_db.msf"))
    Warning: `select_()` is deprecated as of dplyr 0.7.0.
    Please use `select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in UseMethod("select_") : 
      no applicable method for 'select_' applied to an object of class "c('tbl_SQLiteConnection', 'tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl')"
    Calls: make_area_table ... _fseq -> freduce -> withVisible -> <Anonymous> -> select_
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      no applicable method for 'select_' applied to an object of class "c('tbl_SQLiteConnection', 'tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl')"
      Backtrace:
        1. parsemsf::make_area_table(parsemsf_example("test_db.msf"))
        2. dplyr::tbl(my_db, "Events")
       10. dplyr::select_(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 4 ]
      1. Error: make_area_table creates a data frame with the correct column names (@test_make_area_table.R#16) 
      2. Error: make_pep_table creates a data frame with the correct column names (@test_make_pep_table.R#13) 
      3. Error: map_peptides creates a data frame with the correct column names (@test_map_peptides.R#16) 
      4. Error: (unknown) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# pool

<details>

* Version: 0.1.4.3
* GitHub: https://github.com/rstudio/pool
* Source code: https://github.com/cran/pool
* Date/Publication: 2019-10-03 11:30:02 UTC
* Number of recursive dependencies: 46

Run `cloud_details(, "pool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   poolClose(pool)
    + 
    + } else {
    +   message("Please install the 'RSQLite' package to run this example")
    + }
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Error in UseMethod("db_list_tables") : 
      no applicable method for 'db_list_tables' applied to an object of class "c('SQLiteConnection', 'DBIConnection', 'DBIObject')"
    Calls: db_list_tables -> db_list_tables.Pool -> db_list_tables
    Execution halted
    Warning in (function (e)  : You have a leaked pooled object.
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("pool")
      ── 1. Error: pool package: can use dplyr syntax to copy table to DB (@test-dplyr
      no applicable method for 'db_has_table' applied to an object of class "c('SQLiteConnection', 'DBIConnection', 'DBIObject')"
      Backtrace:
       1. testthat::expect_true(db_has_table(pool, "flights"))
       5. pool:::db_has_table.Pool(pool, "flights")
       6. dplyr::db_has_table(db_con, table = table)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 234 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: pool package: can use dplyr syntax to copy table to DB (@test-dplyr.R#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RPresto

<details>

* Version: 1.3.4
* GitHub: https://github.com/prestodb/RPresto
* Source code: https://github.com/cran/RPresto
* Date/Publication: 2019-10-18 17:40:03 UTC
* Number of recursive dependencies: 46

Run `cloud_details(, "RPresto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Error: db_query_fields works with mock  ──────────────────────────────────
      No mocks for url: http://localhost:8000/v1/statement, request_body: SELECT * FROM ((SELECT 1 AS a, 't' AS b) "a") "q01" WHERE 1 = 0
      Backtrace:
       1. dplyr::db_query_fields(...)
       2. RPresto:::db_query_fields.PrestoConnection(...)
       4. RPresto::dbGetQuery(con, fields)
       6. RPresto::dbSendQuery(conn, statement, ...)
       7. httr::POST(url, body = enc2utf8(statement), config = headers)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 229 | SKIPPED: 40 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: db_query_fields works with mock 
      
      Error: testthat unit tests failed
      Execution halted
    ```

