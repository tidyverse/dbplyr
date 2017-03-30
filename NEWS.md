# dbplyr 0.5.0.9000

## New features

* New `src_dbi()` is the new offiicial way to construct dplyr sources with
  a remote backend. `src_dbi()` is very similar to the existing `src_sql()`
  (which remains for backward compatibility), but it recognises that dplyr
  only works with database backends that support the DBI protocol (#2423).
  `src_desc.src_dbi()` dispatches on the connection, eliminating the
  last method that required a dispatch on the class of the src, rather
  than the underlying connection. You can create a `tbl` directly from a 
  DBI connection (#2576).

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

*   `group_by()` can now perform an inline mutate for database backends (#2422).

*   The SQL generation set operations (`intersect()`, `setdiff()`, `union()`, 
    and `union_all()`) have been considerably improved. 
  
    By default, the component SELECT are surrounded with parentheses, except on
    SQLite. The SQLite backend will now throw an error if you attempt a set operation
    on a query that contains a LIMIT, as that is not supported in SQLite (#2270).
    
    All set operations match column names across inputs, filling in non-matching
    variables with NULL (#2556).

*   `rename()` and `group_by()` now combine correctly (#1962)

## Vector-level SQL generation

* Translation of inline scalars:

    * Logical values are now translated to 0 and 1 rather than TRUE and FALSE
      this should work on a wider range of backends (#2052).

    * `Inf` and `-Inf` are correctly escaped

    * Better test for whether or not a double is similar to an integer and 
      hence needs a trailing 0.0 added (#2004).

    * Quoting defaults to `DBI::dbEscapeString()` and `DBI::dbQuoteIdentifier()`
      respectively.

* `::` and `:::` are handled correctly (#2321)

* `x %in% 1` is now correctly translated to `x IN (1)` (#511).

* `ifelse()` and `if_else()` use correct argument names in SQL translation 
  (#2225).

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

* Export `win_over()`, `win_rank()`, `win_recycled()`, and `win_cumulative()`.
  Export `win_current_group()` and `win_current_order()` (#2051, #2126).
  `win_over()` escapes expression using current database rules.

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
