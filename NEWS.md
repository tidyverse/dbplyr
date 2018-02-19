# dbplyr 1.2.1

* Forward compatibility fixes for rlang 0.2.0

# dbplyr 1.2.0

## New top-level translations

* New translations for 
    
    * MS Access (#2946) (@DavisVaughan)
    * Oracle, via odbc or ROracle (#2928, #2732, @edgararuiz)
    * Teradata. 
    * Redshift.

* dbplyr now supplies appropriate translations for the RMariaDB and 
  RPostgres packages (#3154). We generally recommend using these packages
  in favour of the older RMySQL and RPostgreSQL packages as they are
  fully DBI compliant and tested with DBItest.

## New features

* `copy_to()` can now "copy" tbl_sql in the same src, providing another
  way to cache a query into a temporary table (#3064). You can also 
  `copy_to` tbl_sqls from another source, and `copy_to()` will automatically
  collect then copy.

* Initial support for stringr functions: `str_length()`, `str_to_upper()`,
  `str_to_lower()`, `str_replace_all()`, `str_detect()`, `str_trim()`. 
  Regular expression support varies from database to database, but most 
  simple regular expressions should be ok.

## Tools for developers

* `db_compute()` gains an `analyze` argument to match `db_copy_to()`.

* New `remote_name()`, `remote_con()`, `remote_src()`, `remote_query()` and 
  `remote_query_plan()` provide a standard API for get metadata about a 
  remote tbl (#3130, #2923, #2824).

* New `sql_expr()` is a more convenient building block for low-level SQL
  translation (#3169).

* New `sql_aggregate()` and `win_aggregate()` for generating SQL and windowed
  SQL functions for aggregates. These take one argument, `x`, and warn if 
  `na.rm` is not `TRUE` (#3155). `win_recycled()` is equivalent to 
  `win_aggregate()` and has been soft-deprecated.
  
* `db_write_table` now needs to return the table name

## Minor improvements and bug fixes

* Multiple `head()` calls in a row now collapse to a single call. This avoids 
  a printing problem with MS SQL (#3084).

* `escape()` now works with integer64 values from the bit64 package (#3230)

* `if`, `ifelse()`, and `if_else()` now correctly scope the false condition
  so that it only applies to non-NULL conditions (#3157)

* `ident()` and `ident_q()` handle 0-length inputs better, and should
  be easier to use with S3 (#3212)

* `in_schema()` should now work in more places, particularly in `copy_to()` 
   (#3013, @baileych)

* SQL generation for joins no longer gets stuck in a endless loop if you
  request an empty suffix (#3220).

* `mutate()` has better logic for splitting a single mutate into multiple
  subqueries (#3095).

* Improved `paste()` and `paste0()` support in MySQL, PostgreSQL (#3168),
  and RSQLite (#3176). MySQL and PostgreSQL gain support for `str_flatten()` 
  which behaves like `paste(x, collapse = "-")` (but for technical reasons 
  can't be implemented as a straightforward translation of `paste()`).

* `same_src.tbl_sql()` now performs correct comparison instead of always 
  returning `TRUE`. This means that `copy = TRUE` once again allows you to
  perform cross-database joins (#3002).

* `select()` queries no longer alias column names unnecessarily 
  (#2968, @DavisVaughan).

* `select()` and `rename()` are now powered by tidyselect, 
  fixing a few renaming bugs (#3132, #2943, #2860).

* `summarise()` once again performs partial evaluation before database 
  submission (#3148).

* `test_src()` makes it easier to access a single test source.

## Database specific improvements

*   MS SQL
  
    * Better support for temporary tables (@Hong-Revo)
    
    * Different translations for filter/mutate contexts for: `NULL` evaluation
      (`is.na()`, `is.null()`), logical operators (`!`, `&`, `&&`, `|`, `||`),
      and comparison operators (`==`, `!=`, `<`, `>`, `>=`, `<=`)

*   MySQL: `copy_to()` (via `db_write_table()`) correctly translates logical 
    variables to integers (#3151).
  
*   odbc: improved `n()` translation in windowed context.

*   SQLite: improved `na_if` translation (@cwarden)

*   PostgreSQL: translation for `grepl()` added (@zozlak)

# dbplyr 1.1.0

## New features

* `full_join()` over non-overlapping columns `by = character()` translated to
  `CROSS JOIN` (#2924).

* `case_when()` now translates to SQL "CASE WHEN" (#2894)

* `x %in% c(1)` now generates the same SQL as `x %in% 1` (#2898).

* New `window_order()` and `window_frame()` give you finer control over 
  the window functions that dplyr creates (#2874, #2593).

* Added SQL translations for Oracle (@edgararuiz).

## Minor improvements and bug fixes

* `x %in% c(1)` now generates the same SQL as `x %in% 1` (#2898).

* `head(tbl, 0)` is now supported (#2863). 

* `select()`ing zero columns gives a more information error message (#2863).

* Variables created in a join are now disambiguated against other variables
  in the same table, not just variables in the other table (#2823).

* PostgreSQL gains a better translation for `round()` (#60).

* Added custom `db_analyze_table()` for MS SQL, Oracle, Hive and Impala (@edgararuiz)

* Added support for `sd()` for aggregate and window functions (#2887) (@edgararuiz) 

* You can now use the magrittr pipe within expressions,
  e.g. `mutate(mtcars, cyl %>% as.character())`.

* If a translation was supplied for a summarise function, but not for the
  equivalent windowed variant, the expression would be translated to `NULL`
  with a warning. Now `sql_variant()` checks that all aggregate functions 
  have matching window functions so that correct translations or clean errors
  will be generated (#2887)

# dbplyr 1.0.0

## New features

* `tbl()` and `copy_to()` now work directly with DBI connections (#2423, #2576), 
  so there is no longer a need to generate a dplyr src. 
  
    ```R
    library(dplyr)

    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    copy_to(con, mtcars)
    
    mtcars2 <- tbl(con, "mtcars")
    mtcars2
    ```

* `glimpse()` now works with remote tables (#2665)

* dplyr has gained a basic SQL optimiser, which collapses certain nested
  SELECT queries into a single query (#1979). This will improve query
  execution performance for databases with less sophisticated query optimisers,
  and fixes certain problems with ordering and limits in subqueries (#1979).
  A big thanks goes to @hhoeflin for figuring out this optimisation.

* `compute()` and `collapse()` now preserve the "ordering" of rows.
  This only affects the computation of window functions, as the rest
  of SQL does not care about row order (#2281).

* `copy_to()` gains an `overwrite` argument which allows you to overwrite
  an existing table. Use with care! (#2296)

* New `in_schema()` function makes it easy to refer to tables in schema:
  `in_schema("my_schema_name", "my_table_name")`.

## Deprecated and defunct

* `query()` is no longer exported. It hasn't been useful for a while
  so this shouldn't break any code.

## Verb-level SQL generation

* Partial evaluation occurs immediately when you execute a verb (like 
  `filter()` or `mutate()`) rather than happening when the query is executed 
  (#2370).

* `mutate.tbl_sql()` will now generate as many subqueries as necessary so 
  that you can refer to variables that you just created (like in mutate
  with regular dataframes) (#2481, #2483).

* SQL joins have been improved:

    * SQL joins always use the `ON ...` syntax, avoiding `USING ...` even for 
      natural joins. Improved handling of tables with columns of the same name 
      (#1997, @javierluraschi). They now generate SQL more similar to what you'd
      write by hand, eliminating a layer or two of subqueries (#2333)
      
    * [API] They now follow the same rules for including duplicated key variables
      that the data frame methods do, namely that key variables are only
      kept from `x`, and never from `y` (#2410)
      
    * [API] The `sql_join()` generic now gains a `vars` argument which lists
      the variables taken from the left and right sides of the join. If you
      have a custom `sql_join()` method, you'll need to update how your
      code generates joins, following the template in `sql_join.generic()`.
      
    * `full_join()` throws a clear error when you attempt to use it with a
      MySQL backend (#2045)
      
    * `right_join()` and `full_join()` now return results consistent with
      local data frame sources when there are records in the right table with
      no match in the left table. `right_join()` returns values of `by` columns
      from the right table. `full_join()` returns coalesced values of `by` 
      columns from the left and right tables (#2578, @ianmcook)

*   `group_by()` can now perform an inline mutate for database backends (#2422).

*   The SQL generation set operations (`intersect()`, `setdiff()`, `union()`, 
    and `union_all()`) have been considerably improved. 
  
    By default, the component SELECT are surrounded with parentheses, except on
    SQLite. The SQLite backend will now throw an error if you attempt a set operation
    on a query that contains a LIMIT, as that is not supported in SQLite (#2270).
    
    All set operations match column names across inputs, filling in non-matching
    variables with NULL (#2556).

*   `rename()` and `group_by()` now combine correctly (#1962)

*   `tbl_lazy()` and `lazy_tbl()` have been exported. These help you test
    generated SQL with out an active database connection.

*   `ungroup()` correctly resets grouping variables (#2704).

## Vector-level SQL generation

* New `as.sql()` safely coerces an input to SQL.

* More tranlators for `as.character()`, `as.integer()` and `as.double()` 
  (#2775).

* New `ident_q()` makes it possible to specifier identifiers that do not
  need to be quoted.

* Translation of inline scalars:

    * Logical values are now translated differently depending on the backend.
      The default is to use "true" and "false" which is the SQL-99 standard,
      but not widely support. SQLite translates to "0" and "1" (#2052).

    * `Inf` and `-Inf` are correctly escaped

    * Better test for whether or not a double is similar to an integer and 
      hence needs a trailing 0.0 added (#2004).

    * Quoting defaults to `DBI::dbEscapeString()` and `DBI::dbQuoteIdentifier()`
      respectively.

* `::` and `:::` are handled correctly (#2321)

* `x %in% 1` is now correctly translated to `x IN (1)` (#511).

* `ifelse()` and `if_else()` use correct argument names in SQL translation 
  (#2225).

* `ident()` now returns an object with class `c("ident", "character")`. It
   no longer contains "sql" to indicate that this is not already escaped.
   
* `is.na()` and `is.null()` gain extra parens in SQL translation to preserve
  correct precedence (#2302).

* [API] `log(x, b)` is now correctly translated to the SQL `log(b, x)` (#2288).
  SQLite does not support the 2-argument log function so it is translated
  to `log(x) / log(b)`.

* `nth(x, i)` is now correctly translated to `nth_value(x, i)`.

* `n_distinct()` now accepts multiple variables (#2148).

* [API] `substr()` is now translated to SQL, correcting for the difference
  in the third argument. In R, it's the position of the last character,
  in SQL it's the length of the string (#2536).

* `win_over()` escapes expression using current database rules.

## Backends

* `copy_to()` now uses `db_write_table()` instead of `db_create_table()` and 
  `db_insert_into()`. `db_write_table.DBIConnection()` uses `dbWriteTable()`.

* New `db_copy_to()`, `db_compute()` and `db_collect()` allow backends to 
  override the entire database process behind `copy_to()`, `compute()` and 
  `collect()`. `db_sql_render()` allow additional control over the SQL
  rendering process.

* All generics whose behaviour can vary from database to database now 
  provide a DBIConnection method. That means that you can easily scan
  the NAMESPACE to see the extension points.

* `sql_escape_logical()` allows you to control the translation of
  literal logicals (#2614).

* `src_desc()` has been replaced by `db_desc()` and  now dispatches on the 
  connection, eliminating the last method that required dispatch on the class 
  of the src.

* `win_over()`, `win_rank()`, `win_recycled()`, `win_cumulative()`,
  `win_current_group()` and `win_current_order()` are now exported. This
  should make it easier to provide customised SQL for window functions
  (#2051, #2126).
  
*  SQL translation for Microsoft SQL Server (@edgararuiz)

*  SQL translation for Apache Hive (@edgararuiz)

*  SQL translation for Apache Impala (@edgararuiz)


## Minor bug fixes and improvements

* `collect()` once again defaults to return all rows in the data (#1968).
  This makes it behave the same as `as.data.frame()` and `as_tibble()`.

* `collect()` only regroups by variables present in the data (#2156)

* `collect()` will automatically LIMIT the result to the `n`, the number of 
  rows requested. This will provide the query planner with more information
  that it may be able to use to improve execution time (#2083).

* `common_by()` gets a better error message for unexpected inputs (#2091)

* `copy_to()` no longer checks that the table doesn't exist before creation,
  intead preferring to fall back on the database for error messages. This
  should reduce both false positives and false negative (#1470)

* `copy_to()` now succeeds for MySQL if a character column contains `NA` 
   (#1975, #2256, #2263, #2381, @demorenoc, @eduardgrebe).

* `copy_to()` now returns it's output invisibly (since you're often just
   calling for the side-effect).

* `distinct()` reports improved variable information for SQL backends. This
  means that it is more likely to work in the middle of a pipeline (#2359).

* Ungrouped `do()` on database backends now collects all data locally first
  (#2392).

* Call `dbFetch()` instead of the deprecated `fetch()` (#2134).
  Use `DBI::dbExecute()` for non-query SQL commands (#1912)

* `explain()` and `show_query()` now invisibly return the first argument,
  making them easier to use inside a pipeline.

* `print.tbl_sql()` displays ordering (#2287) and prints table name, if known.

* `print(df, n = Inf)` and `head(df, n = Inf)` now work with remote tables 
  (#2580).

* `db_desc()` and `sql_translate_env()` get defaults for DBIConnection.

* Formatting now works by overriding the `tbl_sum()` generic instead of `print()`. This means that the output is more consistent with tibble, and that `format()` is now supported also for SQL sources (tidyverse/dbplyr#14).

## Lazy ops

* [API] The signature of `op_base` has changed to `op_base(x, vars, class)`

* [API] `translate_sql()` and `partial_eval()` have been refined:

    * `translate_sql()` no longer takes a vars argument; instead call
      `partial_eval()` yourself. 
    
    * Because it no longer needs the environment `translate_sql()_` now
      works with a list of dots, rather than a `lazy_dots`.
      
    * `partial_eval()` now takes a character vector of variable names
      rather than a tbl.
      
    * This leads to a simplification of the `op` data structure: 
      dots is now a list of expressions rather than a `lazy_dots`.

* [API] `op_vars()` now returns a list of quoted expressions. This
  enables escaping to happen at the correct time (i.e. when the connection
  is known).
